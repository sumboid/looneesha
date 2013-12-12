package looneesha

import scala.language.dynamics
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.collection.immutable.HashMap

case class ProblemBuilder {
  var dfs: List[AtomDF] = Nil
  var question: List[AtomDF] = Nil

  object quest extends Dynamic {
    def selectDynamic(f: String) = { question ::= AtomDF(f) }
  }

  object defn extends Dynamic {
    def applyDynamic(f: String)(v: Double) = { dfs ::= AtomDF(f, 0, v, true) }
  }
}

