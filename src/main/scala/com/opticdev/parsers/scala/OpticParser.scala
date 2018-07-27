package com.opticdev.parsers.scala

import com.opticdev.parsers
import com.opticdev.parsers.graph.AstType
import com.opticdev.parsers.sourcegear.advanced
import com.opticdev.parsers.sourcegear.basic.BasicSourceInterface
import com.opticdev.parsers._
import com.opticdev.parsers.scala.sourceinterface.advanced.ScalaSourceInterface

import util.Try
import com.opticdev.parsers.utils.Profiling

class OpticParser extends ParserBase {
  override def languageName: String = scalaName

  override def parserVersion: String = "0.0.1"

  override def fileExtensions: Set[String] = Set("scala")

  override def inlineCommentPrefix : String = "#"

  override def programNodeType: graph.AstType = AstType("Source", languageName)

  override def blockNodeTypes: parsers.BlockNodeTypes = BlockNodeTypes(
    BlockNodeDesc(AstType("Template", languageName), "stats"),
    BlockNodeDesc(AstType("Term.Block", languageName), "stats"),
    BlockNodeDesc(AstType("Source", languageName), "stats")
  )

  override def identifierNodeDesc: IdentifierNodeDesc = IdentifierNodeDesc(AstType("Term.Name", languageName), Seq("value"))

  override def basicSourceInterface: BasicSourceInterface = sourceinterface.basic.combined(this)

  override def marvinSourceInterface: advanced.MarvinSourceInterface = ScalaSourceInterface

  override def parseString(contents: String): ParserResult = {
    val timedOperation = Profiling.time[AstGraph] {
      val ast = ScalaCodeToAst.parseCode(contents).get
      ASTJsonToGraph.buildGraphFromJson(ast)
    }

    ParserResult(timedOperation.result, languageName, timedOperation.elapsedTime, this)
  }
}
