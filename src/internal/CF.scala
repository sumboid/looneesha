package looneesha

case class ForCF(name: String, cfs: List[CF], in: (AtomDF, AtomDF), step: AtomDF) extends Fragment {
}

case class CF(name: String, func: List[DF] => List[Double], in: List[DF], out: List[DF]) extends Fragment {
  def run = {
    if (in forall (df => df.define)) {
      val resultDouble = func(in)
      for(i <- 0 until out.size) yield out(i) set resultDouble(i)
    } else Nil
  }

  def set(_in: List[DF]) = CF(name, func, _in, out)
  override def toString = name
}

