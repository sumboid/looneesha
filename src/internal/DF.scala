package looneesha

//case class Counter(name: String, value: Int = 0, defined: Boolean = false) extends Fragment
trait DF {
  def name
}

case class AtomDF(name: String, index: Integer = 0, value: Double = 0, defined: Boolean = false) extends DF {
  override def equals (other: Any) = other match {
    case x: AtomDF => this.name == x.name && this.index == x.index
    case _ => false
  }

  def set (v: Double) = AtomDF(name, index, v, true)
}

case class MetaDF(name: String, metaindex: Int) extends DF
