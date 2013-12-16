package looneesha

import scala.language.dynamics
import scala.language.postfixOps
import scala.language.implicitConversions
import scala.collection.immutable.HashMap

case class GraphBuilder(mapping: Map[String, List[AtomDF] => List[Double]],
                        prefix: String,
                        separator: String = "_") {

  var cfs: List[CF] = Nil
  def addprefix(name: String) = prefix + separator + name

  object defn extends Dynamic {
    def applyDynamic(m: String)(args: => (List[AtomDF], List[AtomDF])) = {
      val (in, out) = args
      cfs ::= AtomCF(addprefix(m), mapping(m), in, out)
    }

    /*def applyDynamic (m: String)(range: => (AtomDF, AtomDF))(args: => (List[DF], List[DF])) = {
      val (in, out) = args
      val (from, to) = range

      cfs ::= MetaRangedСF(prefix(m), mapping(m), from, to, in, out)
    }*/

    def applyDynamic (m: String)(args: => (List[MetaDF], List[MetaDF])) = {
      val (in, out) = args

      cfs ::= MetaСF(addprefix(m), mapping(m), in, out)
    }
  }

  class io extends Dynamic {
    def applyDynamic(f: String)(args: AtomDF*) = args.toList
  }

  object in extends io
  object out extends io

  object % extends Dynamic {
    def fixname(name: String) = name contains separator match {
      case true => name
      case false => addprefix(name)
    }

    def selectDynamic(name: String) = AtomDF(fixname(name))
    def applyDynamic(name: String)(ind: MetaIndex) = MetaDF(fixname(name), ind)
    def applyDynamic(name: String)(ind: Int) = AtomDF(fixname(name), ind)
    implicit def metaToInt (m: MetaIndex) = m.i
  }

  case class MetaIndex(i: Int)

  object * {
    def apply = MetaIndex(0)
    def + (i: Int) = MetaIndex(i)
    def - (i: Int) = MetaIndex(-i)
  }

  def get = Graph(cfs)

  def + (g: GraphBuilder) = {
    val r = new GraphBuilder(new HashMap[String, List[AtomDF] => List[Double]], "no")
    r.cfs = g.cfs ::: this.cfs
    r
  }
}
