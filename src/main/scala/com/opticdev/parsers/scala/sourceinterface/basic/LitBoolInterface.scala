package com.opticdev.parsers.scala.sourceinterface.basic

import com.opticdev.parsers.{AstGraph, ParserBase, graph}
import com.opticdev.parsers.scala.scalaName
import com.opticdev.parsers.graph.{AstType, CommonAstNode}
import com.opticdev.parsers.sourcegear.basic._
import play.api.libs.json.{JsBoolean, JsValue}

class LitBoolInterface extends Literal {
  override val astType: graph.AstType = AstType("Lit.Boolean", scalaName)
  override val parser: SourceParser = (node, graph, raw, basicSourceInterface)=> {
    JsBoolean(raw == "true")
  }
  override val mutator: SourceMutator = (node, graph, raw, newValue, parser, basicSourceInterface) => {
    if (newValue.as[JsBoolean].value) {
      "true"
    } else {
      "false"
    }
  }
  override val generator: SourceGenerator = (newValue, sourceParser, basicSourceInterface) => {
    if (newValue.as[JsBoolean].value) {
      "true"
    } else {
      "false"
    }
  }
}
