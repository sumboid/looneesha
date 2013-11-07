package looneesha

abstract class Fragment {
  def name: String
}

case class DF(name: String, value: Double = 0, define: Boolean = false) extends Fragment {
  override def equals(other: Any) = {
    other.isInstanceOf[DF] && this.name == other.asInstanceOf[DF].name
  }

  def set(v: Double) = DF(name, v, true)
}

case class CF(name: String, func: List[DF] => List[Double], in: List[DF], out: List[DF]) extends Fragment {
  def run = {
    if (in forall (df => df.define)) {
      val resultDouble = func(in)
      for(i <- 0 until out.size) yield out(i) set resultDouble(i)
    }
    Nil
  }

  def set(_in: List[DF]) = CF(name, func, _in, out)
}

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

  def paths(in: List[DF], out: DF): List[Graph] = {
    var result: List[Graph] = Nil

    def _paths(_cfs: List[CF], _dfs: List[DF]): Unit = _dfs.filterNot(in contains _) match {
      case Nil => result ::= Graph(_cfs)
      case dfs => combinations(dfs.map(df => filterOut(cfs, df :: Nil)))
                              .foreach(ncfs => _paths(ncfs ::: _cfs, ncfs.flatMap(_.in)))
    }
    
    _paths(Nil, out :: Nil))

    result
  }


  def subgraph(in: List[DF], out: List[DF]) = Graph(paths(in, out).flatMap(g => g.cfs))

  def filterIn (cfs: List[CF], in: List[DF]) = cfs.filter(_.in forall (in contains _))
  def filterOut (cfs: List[CF], out: List[DF]) = cfs.filter(_.out forall (out contains _))

  def filterIn (in: List[DF]) = cfs.filter(_.in forall (in contains _))
  def filterOut (out: List[DF]) = cfs.filter(_.out forall (out contains _))
}
