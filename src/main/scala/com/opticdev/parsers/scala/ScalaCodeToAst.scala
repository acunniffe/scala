package com.opticdev.parsers.scala
import play.api.libs.json._

import scala.meta._
import scala.util.{Failure, Try}
import prettyprinters._
import inputs._

object ScalaCodeToAst {

  def parseCode(code: String): Try[JsObject] = {

    val parsesAsSource = Try(code.parse[Source].get).map(toNode)

    if (parsesAsSource.isSuccess) {
      return parsesAsSource
    }

    val parsesAsStatement = Try(code.parse[Stat].get).map(toNode)

    if (parsesAsStatement.isSuccess) {
      return parsesAsStatement.map(stmt=> {
        //stub a source so it fits Optic's convention.
        JsObject(Seq(
          "type" ->  JsString("Source"),
          "pos" -> JsObject(Seq(
            "start" -> JsNumber(0),
            "end" -> JsNumber(code.length))),
          "stats" -> JsArray(Seq(stmt)))
        )
      })
    }
    Failure(new Exception("Could not parse scala code "+ parsesAsSource.failed.map(_.getMessage) ))
  }


  //Internals (taken from scalameta package

  private[this] def mergeJSObjects(objs: JsObject*): JsObject = {
    objs.foldLeft(JsObject.empty) {
      case (a, b) => a.deepMerge(b)
    }
  }

  private[this] def toNode(t: Any): JsValue = t match {
    case t: Tree => toNode(t)
    case tt: List[_] => JsArray(tt.map(toNode))
    case t: Option[_] => t.map(toNode).getOrElse(JsNull)
    case _ => JsNull
  }

  private[this] def toPosition(p: Position): JsObject =
    JsObject(Seq(
      "start" -> JsNumber(p.start),
      "end" -> JsNumber(p.end)
    ))

  private[this] def toNode(t: Tree): JsObject = {
    val base = JsObject(Seq(
      "type" -> JsString(t.productPrefix),
      "pos" -> toPosition(t.pos)
    ))

    val fields = JsObject(t.productFields.zip(t.productIterator.toList).collect {
      case (name, value) => name -> toNode(value)
    })


    def v(va: JsValue): JsObject =
      JsObject(Seq("value" -> va))

    val value = t match {
      case Lit.Char(value) => v(JsString(value.toString))
      case Lit.Long(value) => v(JsNumber(value))
      case Lit.Symbol(value) => v(JsString(value.name))
      case Lit(value) => v(JsString(value.toString))
      case Name(value) => v(JsString(value))
      case _ => JsObject.empty
    }

//    val syntax = t match {
//      case _: Lit => js.Dynamic.literal("syntax" -> t.syntax)
//      case _ => js.Dynamic.literal()
//    }

    mergeJSObjects(base, fields, value)
  }

}