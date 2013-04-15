package main

import language.experimental.macros

object Main extends App {

  implicit def stringContextToXmlContext(sc: StringContext) = new XmlContext(sc)

  xml"<foo></foo>>"
}