package looneesha

case class Counter(name: String, value: Int = 0, defined: Boolean = false) extends Fragment {
}

case class AtomDF(name: String, value: Double = 0, defined: Boolean = false) extends Fragment {
  override def equals (other: Any) = {
    other.isInstanceOf[DF] && this.name == other.asInstanceOf[DF].name
  }

  def set (v: Double) = DF(name, v, true)
}

case class DF(name: String) extends Fragment {
  var dfs: Array[AtomDF]
  def apply (i: Int) = i < elems.size match {
    case true => Some(elems(i))
    case false => None
  }
}

