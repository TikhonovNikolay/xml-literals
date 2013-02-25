import language.experimental.macros
import reflect.macros._
import xml.XML

class XmlContext(sc: StringContext) {

  val strContext = sc.parts

  def xml(args: Any) = macro MacrosXml.xml_impl

}

object MacrosXml {

  def xml_impl(c: Context)(args: c.Expr[Any]) : c.Expr[Any] = {

    import c.universe._

    def getListLiteral(tr: Tree): Seq[Any] = tr match {
      case Apply(x, y) => {
        y match {
          case List(tree: Tree) => getListLiteral(tree)
          case _ => y
        }
      }
    }

    val xmlSemantic = getListLiteral(c.prefix.tree).map(
      x => {
        val Literal(Constant(str: String)) = x
        str
      }
    )

    try {
      XML.loadString(xmlSemantic.foldLeft("")((x, y) => x + y))
    } catch {
      case e: org.xml.sax.SAXParseException => {
        c.error(c.enclosingPosition, e.getMessage)
      }
    }

    c.Expr(
      Apply(Select(Select(Select(Ident(newTermName("scala")), newTermName("xml")), newTermName("XML")), newTermName("loadString")),
        List(Apply(Select(Apply(Apply(Select(Apply(Apply(Select(Ident(newTermName("args")), newTermName("zip")), List(Select(c.prefix.tree,
          newTermName("strContext")))), List(Select(reify(scala.collection.Seq).tree, newTermName("canBuildFrom")))),
          newTermName("foldLeft")), List(Literal(Constant("")))), List(Function(List(ValDef(Modifiers(Flag.PARAM), newTermName("x"), TypeTree(),
          EmptyTree), ValDef(Modifiers(Flag.PARAM), newTermName("y"), TypeTree(), EmptyTree)), Apply(Select(Apply(Select(Ident(newTermName("x")),
          newTermName("$plus")), List(Select(Ident(newTermName("y")), newTermName("_2")))), newTermName("$plus")), List(Select(Ident(newTermName("y")),
          newTermName("_1"))))))), newTermName("$plus")), List(Apply(Select(Select(c.prefix.tree, newTermName("strContext")), newTermName("apply")),
          List(Apply(Select(Select(Select(c.prefix.tree, newTermName("strContext")), newTermName("length")), newTermName("$minus")), List(Literal(Constant(1))))))))))
    )
  }
}