package looneesha

abstract class Fragment {
  def name: String
}

case class DF(name: String, value: Double = 0, define: Boolean = false) extends Fragment {
  override def equals(other: Any) = {
    other.isInstanceOf[DF] && this.name == other.asInstanceOf[DF].name
  }
}

case class CF(name: String, in: List[DF], out: List[DF]) extends Fragment

case class Graph(cfs: List[CF]) {
  def combinations (mcfs: List[List[CF]]) = {
    var result: List[List[CF]] = Nil

    def _combinations(_mcfs: List[List[CF]], l: List[CF]): Unit = _mcfs.tail match {
      case Nil => _mcfs.head.foreach(cf => result ::= (cf :: l))
      case xs => _mcfs.head.foreach(cf => _combinations(xs, cf :: l))
    }

    _combinations(mcfs, Nil)
    result
  }

  def paths(in: List[DF], outs: List[DF]): List[Graph] = {
    var result: List[Graph] = Nil

    def _paths(_cfs: List[CF], _dfs: List[DF]): Unit = _dfs.filterNot(in contains _) match {
      case Nil => result ::= Graph(_cfs)
      case dfs => combinations(dfs.map(df => filterOut(cfs, df :: Nil)))
                              .foreach(ncfs => _paths(ncfs ::: _cfs, ncfs.flatMap(_.in)))
    }
    outs.foreach(out => _paths(Nil, out :: Nil))

    result
  }

  def subgraph(in: List[DF], out: List[DF]) = Graph(paths(in, out).flatMap(g => g.cfs))

  def filterIn (cfs: List[CF], in: List[DF]) = cfs.filter(_.in forall (in contains _))
  def filterOut (cfs: List[CF], out: List[DF]) = cfs.filter(_.out forall (out contains _))

  def filterIn (in: List[DF]) = cfs.filter(_.in forall (in contains _))
  def filterOut (out: List[DF]) = cfs.filter(_.out forall (out contains _))
}
