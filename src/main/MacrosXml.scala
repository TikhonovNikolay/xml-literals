package main

import language.experimental.macros
import reflect.macros._

object MacrosXml {

  def xml(sc: StringContext, args: Any*): Any = macro xml_impl

  def xml_impl(c: Context)(sc: c.Expr[StringContext], args: c.Expr[Any]*) = {

    import c.universe._

    c.Expr(
      Apply(
        Select(Select(Select(Ident(newTermName("scala")), newTermName("xml")), newTermName("XML")), newTermName("loadString")),
        List(
          Apply(
            Select(
              Apply(
                Apply(Select(Apply(Apply(Select(Ident(newTermName("args")), newTermName("zip")), List(Select(sc.tree, newTermName("parts")))), List(Select(reify(scala.collection.Seq).tree, newTermName("canBuildFrom")))), newTermName("foldLeft")), List(Literal(Constant("")))),
                List(
                  Function(
                    List(ValDef(Modifiers(Flag.PARAM), newTermName("x"), TypeTree(), EmptyTree), ValDef(Modifiers(Flag.PARAM), newTermName("y"), TypeTree(), EmptyTree)),
                    If(
                      Apply(
                        Select(Apply(Select(Select(Ident(newTermName("y")), newTermName("_2")), newTermName("endsWith")), List(Literal(Constant("=")))), newTermName("$amp$amp")),
                        List(
                          Apply(
                            Select(
                              Apply(
                                Select(
                                  Apply(
                                    Select(
                                      Apply(
                                        Select(reify(scala.Predef).tree, newTermName("augmentString")),
                                        List(Apply(Select(Ident(newTermName("x")), newTermName("$plus")), List(Select(Ident(newTermName("y")), newTermName("_2")))))
                                      ),
                                      newTermName("count")
                                    ),
                                    List(
                                      Function(
                                        List(ValDef(Modifiers(Flag.PARAM), newTermName("p"), TypeTree(), EmptyTree)),
                                        Apply(
                                          Select(Apply(Select(Ident(newTermName("p")), newTermName("equals")),
                                            List(Literal(Constant("<")))), newTermName("$bar$bar")), List(Apply(Select(Ident(newTermName("p")), newTermName("equals")),
                                            List(Literal(Constant(">")))))
                                        )
                                      )
                                    )
                                  ),
                                  newTermName("$percent")),
                                List(Literal(Constant(2)))
                              ), newTermName("$bang$eq")
                            ), List(Literal(Constant(0)))
                          )
                        )
                      ),
                      Apply(
                        Select(
                          Apply(
                            Select(Apply(Select(Apply(Select(Ident(newTermName("x")), newTermName("$plus")), List(Select(Ident(newTermName("y")), newTermName("_2")))), newTermName("$plus")), List(Literal(Constant("\"")))), newTermName("$plus")),
                            List(Select(Ident(newTermName("y")), newTermName("_1")))
                          ),
                          newTermName("$plus")
                        ), List(Literal(Constant("\"")))),
                      Apply(Select(Apply(Select(Ident(newTermName("x")), newTermName("$plus")), List(Select(Ident(newTermName("y")), newTermName("_2")))), newTermName("$plus")), List(Select(Ident(newTermName("y")), newTermName("_1"))))
                    )
                  )
                )
              ),
              newTermName("$plus")
            ),
            List(
              Apply(
                Select(Select(Ident(newTermName("sc")), newTermName("parts")), newTermName("apply")),
                List(Apply(Select(Select(Select(sc.tree, newTermName("parts")), newTermName("length")), newTermName("$minus")), List(Literal(Constant(1)))))
              )
            )
          )
        )
      )
    )
  }
}
