package looneesha

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

  def paths(in: List[DF], outs: List[DF]) = {
    var result: List[Graph] = Nil

    def hasLoop(_cfs: List[CF], _ncfs: List[CF]) = _ncfs flatMap (_.in) filter (_cfs flatMap (_.out) contains _) match {
      case Nil => false
      case _ => true
    }

    def _paths(_cfs: List[CF], _dfs: List[DF], _edfs: List[DF]): Unit = { _dfs.filterNot(_edfs contains _) match {
    case Nil => result ::= Graph(_cfs)
    case dfs => combinations(dfs map (df => filterOut(df :: Nil)))
                              .filterNot(hasLoop(_cfs, _))
                              .foreach(ncfs => _paths(ncfs ::: _cfs, ncfs.flatMap(_.in), dfs ::: _edfs))
    }}
    outs.foreach(out => _paths(Nil, out :: Nil, in))

    result
  }

  def paths(in: List[DF], out: DF): List[Graph] = paths(in, out :: Nil)
  def subgraph(in: List[DF], out: List[DF]) = Graph(paths(in, out) flatMap (_.cfs))

  def filterIn (cfs: List[CF], in: List[DF]) = cfs.filter(_.in forall (in contains _))
  def filterOut (cfs: List[CF], out: List[DF]) = cfs.filter(_.out forall (out contains _))

  def filterIn (in: List[DF]) = cfs.filter(_.in forall (in contains _))
  def filterOut (out: List[DF]) = cfs.filter(_.out forall (out contains _))

  def specialContains (cf: CF, in: List[DF]) = (in filter (cf.in contains _)) != Nil
  def specialFilterIn (in: List[DF]) = cfs filter (specialContains(_, in))
}
