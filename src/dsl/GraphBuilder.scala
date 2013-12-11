package looneesha

import scala.language.dynamics
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.collection.immutable.HashMap

case class GraphBuilder(mapping: Map[String, List[DF] => List[Double]],
                        prefix: String,
                        separator: String = "_") {

  var cfs: List[CF] = Nil
  var scfs: List[SpecCF] = Nil
  object defn extends Dynamic {
    def applyDynamic(m: String)(args: => (List[AtomDF], List[AtomDF])) = {
      val (in, out) = args
      cfs ::= CF(prefix + separator + m, mapping(m), in, out)
    }
  }

  class io extends Dynamic {
    def applyDynamic(f: String)(args: AtomDF*) = args.toList
  }

  object in extends io
  object out extends io

  // AtomDF
  object % extends Dynamic {
    def selectDynamic(name: String) = name contains separator match {
      case true  => AtomDF(name)
      case false => AtomDF(prefix + separator + name)
    }

    def applyDynamic(name: String)(id: String) = selectDynamic(name)
  }

  object * extends Dynamic {
    def selectDynamic(name: String) = name
  }

  object FOR {
    def apply (range: (Counter, Counter)) (_cfs: () => List[CF]) = cfs ::= For(range, _cfs)
  }

  object IF {
    def apply (cond: CF) = If(cond, cond.in, cond.out)
  }

  def get = Graph(cfs)

  def + (g: GraphBuilder) = {
    val r = new GraphBuilder(new HashMap[String, List[DF] => List[Double]], "no")
    r.cfs = g.cfs ::: this.cfs
    r
  }
}
