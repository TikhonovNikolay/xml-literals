package main.scala

import xml.XML

import scala.reflect.macros.Context

object MacrosXml {

    def xml(sc: StringContext, args: Any*): Unit = macro xml_impl

    def validateParameter(str : Any) = {
      if (str.isInstanceOf[String]) {
        str.asInstanceOf[String].replace("<", "&lt;").replace(">", "&gt;")
      } else {
        str
      }
    }

    def xml_impl(c: Context)(sc: c.Expr[StringContext], args: c.Expr[Any]*): c.Expr[Unit] = {
      sc.checkLengths(args)
      XML.loadString(
        args.zip(sc.parts).foldLeft("")
          ((x, y) => {
            if (y._2.endsWith("=") && (x + y._2).count(p => p.equals('<') || p.equals('>')) % 2 != 0) {
              x + y._2 + "\"" + validateParameter(y._1) + "\""
            } else {
              x + y._2 + validateParameter(y._1)
            }
          })
          + sc.parts.apply(sc.parts.length - 1)
      )
    }
}
