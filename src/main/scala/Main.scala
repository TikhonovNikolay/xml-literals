package main.scala

import xml.XML

class XmlContext(sc: StringContext) {

  def xml(args: Any*) = MacrosXml.xml(sc, args)

}

object Main extends App {

  implicit def stringContextToXmlContext(sc: StringContext) = new XmlContext(sc)

  val V1 = 1
  val V2 = 2
  val V3 = 3
  val V4 = "</a> 4 <a>"
  println( xml"""<html><testParameter par1=$V1 par2=$V2 par3=$V3 par4=$V4 /><body>There is no quote=$V1</body></html>""")
  val xs = List(1, 2, 3, 4, 5)
  println( xml"""<for>${(for (x <- xs) yield xml"""<bar x=$x/>""").foldLeft("")((x, y) => x + y)}</for>""")
}