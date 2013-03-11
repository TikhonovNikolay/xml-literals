import language.experimental.macros
import reflect.macros._
import xml.XML

class XmlContext(val sc: StringContext) {
  def xml(args: Any*) = macro MacrosXml.xml_impl
}

object MacrosXml {

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

    validateXml(c, xmlConstantPart.asInstanceOf[Seq[String]])

    val exprForStringLiterals =
      c.Expr(
        Apply(
          Ident(newTermName("List")),
          xmlConstantPart.asInstanceOf[Seq[String]].collect{case s: String => Literal(Constant(s))}.asInstanceOf[List[Tree]]
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