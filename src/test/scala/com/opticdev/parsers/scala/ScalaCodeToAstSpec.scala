package com.opticdev.parsers.scala

import org.scalatest.FunSpec
import play.api.libs.json.JsString

import scala.meta.Source
import scala.meta._

class ScalaCodeToAstSpec extends FunSpec {

  it("can parse file level code") {
    val code =
      """
        |object Main {
        |  def main(args: Array[String]): Unit = {
        |    println("Hello, World!")
        |    goto("place")
        |  }
        |}
        |"""
      .stripMargin

    assert(ScalaCodeToAst.parseCode(code).isSuccess)
  }

  it("can parse statement level code / wrap in Source node") {
    val code =
      """
        |val hello = "WORLD"
        |"""
        .stripMargin

    val astTry = ScalaCodeToAst.parseCode(code)

    assert(astTry.isSuccess)
    assert((astTry.get \ "type").get == JsString("Source"))
  }

  it("will fail with invalid code") {
    assert(ScalaCodeToAst.parseCode("class class").isFailure)
  }

}
