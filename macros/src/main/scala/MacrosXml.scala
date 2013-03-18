import language.experimental.macros
import reflect.macros._
import xml.XML

class XmlContext(val sc: StringContext) {
  def xml(args: Any*) = macro MacrosXml.xml_impl
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

  def validateXml(c: Context, xmlPart: Seq[String]) = {
    try {
      XML.loadString(xmlPart.mkString)
    } catch {
      case e: org.xml.sax.SAXParseException => {
        c.error(
          c.enclosingPosition.withPoint(c.enclosingPosition.point + e.getColumnNumber + 3),
          e.getMessage
        )
      }
    }
  }

  def xml_impl(c: Context)(args: c.Expr[Any]*): c.Expr[Any] = {

    import c.universe._

    val xmlConstantPart = c.prefix.tree match {
      case Apply(_, List(Apply(_, stringLiteralConstant: List[Tree]))) => stringLiteralConstant.collect {
        case Literal(Constant(str: String)) => str
      }
      case _ => c.error(c.enclosingPosition, "Invalid call XML macros")
    }

    val xmlConstantPartWithQuotesAttribution = attributeQuotes(List(xmlConstantPart.asInstanceOf[Seq[String]]: _*), List(), false, false)

    validateXml(c, xmlConstantPartWithQuotesAttribution)

    val exprForStringLiterals =
      c.Expr(
        Apply(
          Ident(newTermName("List")),
          xmlConstantPartWithQuotesAttribution.collect{case s: String => Literal(Constant(s))}.asInstanceOf[List[Tree]]
        )
      )

    val treeForListParameters =
      c.Expr(
        Apply(
          Ident(newTermName("List")),
          args.collect{case e: Expr[Any] => e.tree}.asInstanceOf[List[Tree]]
        )
      )

    reify {
      XML.loadString(
        exprForStringLiterals.splice.asInstanceOf[List[String]].zip(treeForListParameters.splice.asInstanceOf[List[Any]]).foldLeft("") {(x, y) => x + y._1 + y._2}
          + exprForStringLiterals.splice.asInstanceOf[List[String]](exprForStringLiterals.splice.asInstanceOf[List[String]].size - 1)
      )
    }
  }
}