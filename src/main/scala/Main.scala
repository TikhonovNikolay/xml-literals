package main.scala

import xml.XML

class XmlContext(sc: StringContext) {
  def xml(args: Any*) = {
    sc.checkLengths(args)
    XML.loadString(
      args.zip(sc.parts).foldLeft("")
        ((x, y) => {
          if (y._2.endsWith("=") && (x + y._2).count(p => p.equals('<') || p.equals('>')) % 2 != 0) {
            x + y._2 + "\"" + y._1 + "\""
          } else {
            x + y._2 + y._1
          }
        })
        + sc.parts.apply(sc.parts.length - 1)
    )
  }
}

object Main extends App {

  implicit def stringContextToXmlContext(sc: StringContext) = new XmlContext(sc)

  val V1 = 1
  val V2 = 2
  val V3 = 3
  val V4 = 4
  println( xml"""<html><testParameter par1=$V1 par2=$V2 par3=$V3 par4=$V4 /><body>There is no quote=$V1</body></html>""")
  val xs = List(1, 2, 3, 4, 5)
  println( xml"""<for>${(for (x <- xs) yield xml"""<bar x=$x/>""").foldLeft("")((x, y) => x + y)}</for>""")
}