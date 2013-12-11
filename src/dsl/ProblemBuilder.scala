package looneesha

import scala.language.dynamics
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.collection.immutable.HashMap

case class ProblemBuilder {
  var dfs: List[DF] = Nil
  var question: List[DF] = Nil

  object quest extends Dynamic {
    def selectDynamic(f: String) = { question ::= DF(f) }
  }

  object defn extends Dynamic {
    def applyDynamic(f: String)(v: Double) = { dfs ::= DF(f, v, true) }
  }
}

