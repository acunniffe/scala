package com.opticdev.parsers.scala

import org.scalatest.FunSpec

class ASTJsonToGraphSpec extends FunSpec {

  lazy val astjson = ScalaCodeToAst.parseCode(
    """
      |object Test {
      | def me = you
      |}
    """.stripMargin).get

  it("can turn AST to a Graph") {
    val graph = ASTJsonToGraph.buildGraphFromJson(astjson)
    assert(graph.nodes.size == 9)
    assert(graph.edges.size == 8)
  }

}
