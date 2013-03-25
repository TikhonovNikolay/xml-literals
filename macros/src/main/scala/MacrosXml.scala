import language.experimental.macros
import reflect.macros._
import xml.XML

class XmlContext(val sc: StringContext) {
  def xml(args: Any*) = macro MacrosXml.xml_impl
}

class ValidateXml(val c: Context) {

  import c.universe._

  def calculationErrorPosition(e: org.xml.sax.SAXParseException, xmlPart: Seq[String], sourceXmlPartTree: List[Tree]) = {

    def findPartWithError(xmlPart: Seq[String], length: Int, number: Int): Int = xmlPart match {
      case h :: t =>
        if (h.length + length >= e.getColumnNumber) {
          number
        } else {
          findPartWithError(t, h.length + length, number + 1)
        }
      case Nil => {
        c.error(sourceXmlPartTree.apply(sourceXmlPartTree.size - 1).pos, "Invalid xml")
        throw new RuntimeException("Invalid xml")
      }
    }

    val indexOfInvalidPart = findPartWithError(xmlPart, 0, 0)

    val Literal(Constant(sourceString: String)) = sourceXmlPartTree(indexOfInvalidPart)

    (xmlPart(indexOfInvalidPart).length == sourceString.length) match {
      case true => sourceXmlPartTree(indexOfInvalidPart).pos
        .withPoint(sourceXmlPartTree(indexOfInvalidPart).pos.point + e.getColumnNumber - sourceString.length)
      case false => if (sourceString.startsWith(xmlPart(indexOfInvalidPart))) {
        sourceXmlPartTree(indexOfInvalidPart).pos
          .withPoint(sourceXmlPartTree(indexOfInvalidPart).pos.point + e.getColumnNumber - sourceString.length)
      } else {
        sourceXmlPartTree(indexOfInvalidPart).pos
          .withPoint(sourceXmlPartTree(indexOfInvalidPart).pos.point + e.getColumnNumber - sourceString.length - 1)
      }
    }
  }

  def validate(c: Context, xmlPart: Seq[String], sourceXmlPartTree: List[Tree]) = {
    try {
      XML.loadString(xmlPart.mkString)
    } catch {
      case e: org.xml.sax.SAXParseException => {
        c.error(
          calculationErrorPosition(e, xmlPart, sourceXmlPartTree),
          e.getMessage
        )
      }
    }
  }
}

object MacrosXml {

  def calculateInner(str: String, inner: Boolean): Boolean = str match {
    case "" => inner
    case _ if (str.startsWith(">") && inner) => calculateInner(str.substring(1), false)
    case _ if (str.startsWith("<") && !inner) => calculateInner(str.substring(1), true)
    case _ => calculateInner(str.substring(1), inner)
  }

  def attributeQuotes(xmlParts: List[String], xmlScreen: List[String], inner: Boolean, next: Boolean): List[String] = xmlParts match {
    case h :: t => {
      val in = calculateInner(h, inner)
      if (in && h.endsWith("=")) {
        attributeQuotes(t, xmlScreen :+ (if (next) "\"" else "") + h + "\"", true, true)
      } else {
        attributeQuotes(t, xmlScreen :+ (if (next) "\"" else "") + h, in, false)
      }
    }
    case Nil => xmlScreen
  }

  def xml_impl(c: Context)(args: c.Expr[Any]*): c.Expr[Any] = {

    import c.universe._

    val xmlConstantPartTree = (c.prefix.tree match {
      case Apply(_, List(Apply(_, stringLiteralConstant: List[Tree]))) => stringLiteralConstant
      case _ => c.error(c.enclosingPosition, "Invalid call XML macros")
    }).asInstanceOf[List[Tree]]

    val xmlConstantPart = xmlConstantPartTree.collect {
      case Literal(Constant(str: String)) => str
    }

    val xmlConstantPartWithQuotesAttribution = attributeQuotes(List(xmlConstantPart.asInstanceOf[Seq[String]]: _*), List(), false, false)

    val utilValidate = new ValidateXml(c)
    utilValidate.validate(c, xmlConstantPartWithQuotesAttribution, xmlConstantPartTree.asInstanceOf)

    val exprForStringLiterals =
      c.Expr(
        Apply(
          Ident(newTermName("List")),
          xmlConstantPartWithQuotesAttribution.collect {
            case s: String => Literal(Constant(s))
          }.asInstanceOf[List[Tree]]
        )
      )

    val treeForListParameters =
      c.Expr(
        Apply(
          Ident(newTermName("List")),
          args.collect {
            case e: Expr[Any] => e.tree
          }.asInstanceOf[List[Tree]]
        )
      )

    reify {
      XML.loadString(
        exprForStringLiterals.splice.asInstanceOf[List[String]].zip(treeForListParameters.splice.asInstanceOf[List[Any]]).foldLeft("") {
          (x, y) => x + y._1 + y._2
        }
          + exprForStringLiterals.splice.asInstanceOf[List[String]](exprForStringLiterals.splice.asInstanceOf[List[String]].size - 1)
      )
    }
  }
}
