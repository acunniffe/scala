package com.opticdev.parsers.scala.sourceinterface

import com.opticdev.parsers.graph.CommonAstNode
import com.opticdev.parsers.scala.sourceinterface.basic.{LitBoolInterface, LitIntInterface, LitStringInterface, LitSymbolInterface}
import org.scalatest.FunSpec
import play.api.libs.json._

class BasicSourceInterfaceSpec extends FunSpec {
  describe("LitBoolInterface") {
    lazy val litBoolInterface = new LitBoolInterface

    it("can parse") {
      assert(litBoolInterface.parser(null, null, "true", null) == JsBoolean(true))
      assert(litBoolInterface.parser(null, null, "false", null) == JsBoolean(false))
    }

    it("can mutate") {
      assert(litBoolInterface.mutator(null, null, null, JsBoolean(true), null, null) == "true")
      assert(litBoolInterface.mutator(null, null, null, JsBoolean(false), null, null) == "false")
    }

    it("can generate") {
      assert(litBoolInterface.generator(JsBoolean(true), null, null) == "true")
      assert(litBoolInterface.generator(JsBoolean(false), null, null) == "false")
    }
  }

  describe("Number Interface") {
    describe("Int Interface") {

      lazy val intInterface = new LitIntInterface

      def node(int: Int) = CommonAstNode(null, null, Json.parse(s"""{"value": $int}""").as[JsObject])

      it("can parse") {
        assert(intInterface.parser(node(24), null, null, null) == JsNumber(24))
      }

      it("can mutate") {
        assert(intInterface.mutator(null, null, null, JsNumber(45), null, null) == "45")
      }

      it("can generate") {
        assert(intInterface.generator(JsNumber(12), null, null) == "12")
      }

    }
  }

  describe("String Interface") {

    lazy val stringInterface = new LitStringInterface

    def node(string: String) = CommonAstNode(null, null, Json.parse(s"""{"value": "${string}"}""").as[JsObject])

    it("can parse") {
      assert(stringInterface.parser(node("abcdef"), null, null, null) == JsString("abcdef"))
    }

    it("can mutate") {
      assert(stringInterface.mutator(null, null, null, JsString("abc"), null, null) == "\"abc\"")
    }

    it("can generate") {
      assert(stringInterface.generator(JsString("abc"), null, null) == "\"abc\"")
    }

  }

  describe("Symbol Interface") {

    lazy val symbolInterface = new LitSymbolInterface

    def node(string: String) = CommonAstNode(null, null, Json.parse(s"""{"value": "${string}"}""").as[JsObject])

    it("can parse") {
      assert(symbolInterface.parser(node("abcdef"), null, null, null) == JsString("abcdef"))
    }

    it("can mutate") {
      assert(symbolInterface.mutator(null, null, null, JsString("abc"), null, null) == "'"+"abc")
    }

    it("can generate") {
      assert(symbolInterface.generator(JsString("abc"), null, null) == "'"+"abc")
    }

  }

}
