package com.opticdev.parsers.scala

import com.opticdev.parsers.graph.AstType
import org.scalatest.FunSpec

class ASTJsonToGraphSpec extends FunSpec {

  lazy val astjson = ScalaCodeToAst.parseCode(
    """
      |object Test {
      | def me = you
      |}
    """.stripMargin).get

  it("can turn simple AST to a Graph") {
    val graph = ASTJsonToGraph.buildGraphFromJson(astjson)
    assert(graph.nodes.size == 9)
    assert(graph.edges.size == 8)
  }


  lazy val virtualNodeExample = ScalaCodeToAst.parseCode(
    """
      |
      |object WebServer {
      |  def main(args: Array[String])(test: String)(hello: TypeValue) {
      |
      |  }
      |}
      |
    """.stripMargin).get

  it("works when virtual nodes are needed in the Graph") {
    val graph = ASTJsonToGraph.buildGraphFromJson(virtualNodeExample)
    val definition = graph.nodes.find(_.value.isASTType(AstType("Defn.Def", "scala"))).get
    val children = definition.labeledDependents(graph)

    val virtualParams = children.filter(_._2.isASTType(AstType("virtual_paramss", "scala"))).map(_._2)

    assert(virtualParams.size == 3)

    val flattenedTotalParams = virtualParams.flatMap(_.dependents(graph))

    assert(flattenedTotalParams.size == 3)

  }


}
