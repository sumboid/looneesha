package looneesha

case class Counter(name: String, value: Int = 0, defined: Boolean = false) extends Fragment {
}

case class DF(name: String, index: Integer, value: Double = 0, defined: Boolean = false) extends Fragment {
  override def equals (other: Any) = {
    other.isInstanceOf[DF] &&
    this.name == other.asInstanceOf[DF].name &&
    this.index == other.asInstanceOf[DF].index
  }

  def set (v: Double) = DF(name, index, v, true)
}
