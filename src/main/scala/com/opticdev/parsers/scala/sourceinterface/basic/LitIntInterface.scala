package com.opticdev.parsers.scala.sourceinterface.basic

import com.opticdev.parsers.graph
import com.opticdev.parsers.graph.AstType
import com.opticdev.parsers.scala.scalaName
import com.opticdev.parsers.sourcegear.basic._
import play.api.libs.json.{JsBoolean, JsNumber}

class LitIntInterface extends Literal {
  override val astType: graph.AstType = AstType("Lit.Int", scalaName)
  override val parser: SourceParser = (node, graph, raw, basicSourceInterface)=> {
    (node.properties \ "value").get.as[JsNumber]
  }
  override val mutator: SourceMutator = (node, graph, raw, newValue, parser, basicSourceInterface) => {
    newValue.as[JsNumber].value.toString().replaceAll("\"", "")
  }
  override val generator: SourceGenerator = (newValue, sourceParser, basicSourceInterface) => {
    newValue.as[JsNumber].value.toString().replaceAll("\"", "")
  }
}
