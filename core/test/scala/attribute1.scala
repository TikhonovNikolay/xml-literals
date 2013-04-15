package main

import language.experimental.macros

object Main extends App {

  implicit def stringContextToXmlContext(sc: StringContext) = new XmlContext(sc)

  xml"<foo bar=$V1 bar1=1 />"
}