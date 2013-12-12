package looneesha

case class CF(name: String, func: List[AtomDF] => List[Double], in: List[AtomDF], out: List[AtomDF]) extends Fragment {
  def run = {
    if (in forall (df => df.defined)) {
      val resultDouble = func(in)
      for(i <- 0 until out.size) yield out(i) set resultDouble(i)
    } else Nil
  }

  def set(_in: List[AtomDF]) = CF(name, func, _in, out)
  override def toString = name
}

