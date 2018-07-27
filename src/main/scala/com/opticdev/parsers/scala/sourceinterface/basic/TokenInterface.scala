package com.opticdev.parsers.scala.sourceinterface.basic

import com.opticdev.parsers.graph.AstType
import com.opticdev.parsers.sourcegear.basic.{SourceGenerator, SourceMutator, SourceParser, Token}
import play.api.libs.json.JsString
import com.opticdev.parsers.scala.scalaName

class TokenInterface extends Token {
  override val astType: AstType = AstType("Term.Name", scalaName)

  //@todo regex is not conclusive
  override val parser: SourceParser = (node, graph, raw, basicSourceInterface)=> {
    JsString(raw)
  }
  override val mutator: SourceMutator = (node, graph, raw, newValue, sourceParser, basicSourceInterface) => {
    generator(newValue, sourceParser, basicSourceInterface)
  }

  override val generator : SourceGenerator = (newValue, sourceParser, basicSourceInterface) => {
    val value = newValue.as[JsString].value
    if (isValidValue(value)) {
      value
    } else throw new Error("Invalid Term.Name format.")
  }

  override def isValidValue(value: String): Boolean = true
}
