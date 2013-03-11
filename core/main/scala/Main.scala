package main

import scala.reflect.runtime.universe._
import language.experimental.macros

object Main extends App {

  implicit def stringContextToXmlContext(sc: StringContext) = new XmlContext(sc)

  val V1 = "444555"
  val V2 = "3"
  val V3 = "5"
  val V4 = "444"

  println( xml"<html>$V1</html>" )
  println( xml"""<html><testParameter par1="$V1"/><body>There is no quote=$V1</body></html>""")
  println( xml"""<html><testParameter error par1="$V1" par2="$V2" par3="$V3" par4="$V4" /><body>There is no quote=$V1</body></html>""")
}