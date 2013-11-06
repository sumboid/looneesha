package looneesha

object Test extends GraphBuilder {
	defn a in ("x") -> out ("y")
	defn b in ("x", "t") -> out ("y")
	defn c in ("y", "p") -> out ("f") 
}

object Main {
  def main(args: Array[String]): Unit = {
    GV create (Test.get) draw
  }
}
