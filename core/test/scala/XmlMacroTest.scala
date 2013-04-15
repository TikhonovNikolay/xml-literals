import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter

class XmlMacroTest extends FunSuite {

  implicit def stringContextToXmlContext(sc: StringContext) = new XmlContext(sc)

  test("Xml macros must be equals xml literal"){
    assert(<foo></foo> == xml"<foo></foo>")
    assert(<foo><bar1 /><bar2 /><bar3 /><bar4 /></foo> == xml"<foo><bar1 /><bar2 /><bar3 /><bar4 /></foo>")
  }

  test("Attributes must be in quotes"){
    val atr1 = "foo"
    assert(<bar atr ="foo" /> == xml"<bar atr =$atr1 />")

    val atr1 = "foo1"
    val atr2 = "foo2"
    val atr3 = "foo3"

    assert(<bar atr1 ="foo1" atr2 ="foo2" atr3 ="foo3" />
      == xml"<bar atr1 =$atr1 atr2 =$atr2 atr3 =$atr3 />")
  }

  test("XML should support nesting"){
    val xs = List(1, 2, 3, 4, 5)

    assert(<for><bar x="1"/><bar x="2"/><bar x="3"/><bar x="4"/><bar x="5"/></for> ==
      xml"""<for>${(for (x <- xs) yield xml"""<bar x=$x/>""").foldLeft("")((x, y) => x + y)}</for>""")
  }

}
