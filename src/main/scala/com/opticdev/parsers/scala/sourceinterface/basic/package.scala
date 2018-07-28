package com.opticdev.parsers.scala.sourceinterface

import com.opticdev.parsers.ParserBase
import com.opticdev.parsers.sourcegear.basic._

package object basic {
  def combined(implicit sourceParser: ParserBase) = new BasicSourceInterface {
    override val literals =
      LiteralInterfaces(
        new LitBoolInterface,
        new LitIntInterface,
        new LitStringInterface,
        new LitSymbolInterface
      )
    override val tokens: TokenInterfaces =
      TokenInterfaces(new TokenInterface)
//    override val objectLiterals: ObjectLiteralsInterfaces = ObjectLiteralsInterfaces()
//    override val arrayLiterals: ArrayLiteralsInterfaces = ArrayLiteralsInterfaces()
  }
}
