package com.opticdev.parsers.scala

import org.scalatest.FunSpec

class ScalaParserSpec extends FunSpec {

  lazy val opticParser = new OpticParser

  it("can parse a single scala file") {
    println(opticParser.parseString("hello = world").elapsedTime)
  }

  it("can parse many files in parallel") {
    val parseIssues = (0 to 1000)
      .par
      .map(i=> opticParser.parseString(s"number = $i").graph.toString()
      .contains(i.toString))
      .exists(_ == false)
    assert(!parseIssues)
  }

}
