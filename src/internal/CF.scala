package looneesha

trait CF extends Fragment {
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

case class MetaCF(name: String,
  func: List[AtomDF] => List[Double],
  in: List[DF],
  out: List[DF]) extends CF {

  def createAtomCF(df: DF) = {
    val neededDF = (out find (_ == df)).get.asInstanceOf[MetaDF]
    val startInd = df.asInstanceOf[AtomDF].index - neededDF.metaindex
    val concreteIn = in map (df => df match
      { case x: AtomDF => x.asInstanceOf[AtomDF]
        case x: MetaDF => x.createAtomDF(startInd)})

    val conсreteOut = out map (df => df match
      { case x: AtomDF => x.asInstanceOf[AtomDF]
        case x: MetaDF => x.createAtomDF(startInd) })
    AtomCF(name, func, concreteIn, conсreteOut)
  }
}
