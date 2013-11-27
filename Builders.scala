package looneesha

import scala.language.dynamics
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.collection.immutable.HashMap

case class GraphBuilder(mapping: Map[String, List[DF] => List[Double]], prefix: String, separator: String = "_") {
  var cfs: List[CF] = Nil

  object defn extends Dynamic {
    def applyDynamic(m: String)(args: => (List[DF], List[DF])) = { 
      val (in, out) = args 
      cfs ::= CF(prefix + separator + m, mapping(m), in, out)
    } 
  }

  class io extends Dynamic {
    def applyDynamic(f: String)(args: String*) = { args.toList map (name => if (name contains separator) DF(name) else DF(prefix + separator + name)) }
  }

  object in extends io
  object out extends io

  def get = Graph(cfs) 

  def + (g: GraphBuilder) = { 
    val r = new GraphBuilder(new HashMap[String, List[DF] => List[Double]], "no")
    r.cfs = g.cfs ::: this.cfs
    r
  }
}

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
