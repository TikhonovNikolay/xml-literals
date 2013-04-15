import language.experimental.macros

object Main extends App {

  implicit def stringContextToXmlContext(sc: StringContext) = new XmlContext(sc)

  xml"<html bar = 1></html>"
}