package main

import xml.XML
import scala.reflect.runtime.universe._

class XmlContext(sc: StringContext) {

  def xml(args: Any*) = MacrosXml.xml(sc, args)

}

object Main extends App {

  implicit def stringContextToXmlContext(sc: StringContext) = new XmlContext(sc)

  val V1 = 1
  val V2 = 2
  println(xml"""<html a = "$V2">$V1</html>""")
  println(xml"""<html a = $V2>$V1</html>""")
}