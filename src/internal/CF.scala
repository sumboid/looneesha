package looneesha

trait CF {
  def name: String
  def in: List[DF]
  def out: List[DF]
  def func: List[AtomDF] => List[Double]
}

case class AtomCF(name: String,
                  func: List[AtomDF] => List[Double],
                  in: List[AtomDF],
                  out: List[AtomDF]) extends CF {
  def run = {
    if (in forall (df => df.defined)) {
      val resultDouble = func(in)
      for(i <- 0 until out.size) yield out(i) set resultDouble(i)
    } else Nil
  }

  def set(_in: List[AtomDF]) = AtomCF(name, func, _in, out)
  override def toString = name
}

case class MetaRangedCF(name: String,
                  func: List[AtomDF] => List[Double],
                  from: AtomDF,
                  to: AtomDF,
                  in: List[DF],
                  out: List[DF]) extends CF {
}

case class MetaCF(name: String,
                  func: List[AtomDF] => List[Double],
                  in: List[DF],
                  out: List[DF]) extends CF {
}
