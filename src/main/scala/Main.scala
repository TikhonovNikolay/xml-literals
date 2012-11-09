package main.scala

import xml.XML

class XmlContext(sc: StringContext) {
  def xml(args: Any*) = {
    sc.checkLengths(args)
    XML.loadString(sc.parts.head + args.zip(sc.parts.tail).foldLeft("")((x, y) => x + y._1 + y._2))
  }
}

object Main extends App {

  implicit def stringContextToXmlContext(sc: StringContext) = new XmlContext(sc)

  val title = "Some text"
  val body = 5555
  val someXmlBlock = <innerBlock size ="100500">00000</innerBlock>
  println(xml"<html><tile>$title</tile><body>$body $someXmlBlock</body></html>")
  println(xml"<sdfsdf<html><tile>$title</tile><body>$body</body></html>")

}