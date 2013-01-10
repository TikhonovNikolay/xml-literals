package main



class XmlContext(sc: StringContext) {

  def xml(args: Any*) = {
    RuntimeXml.xml(sc, args)
  }

}

object Main extends App {

  implicit def stringContextToXmlContext(sc: StringContext) = new XmlContext(sc)

  val V1 = "444555"
  val V2 = 2
  val V3 = 3
  val V4 = "444"
  println( xml"""<html><testParameter par1="$V1" par2="$V2" par3="$V3" par4="$V4" /><body>There is no quote=$V1</body></html>""")
}