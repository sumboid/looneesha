package looneesha
import scala.collection.immutable.HashMap

object TestDefinition extends CFDefinition {
	val mapping = Map("a" -> a _, "b" -> b _, "c" -> c _)

	def a (in: List[DF]) = (in(0) + 1) :: Nil
	def b (in: List[DF]) = (in(0) + in(1)) :: Nil
	def c (in: List[DF]) = (in(0) - in(1)) :: Nil 
}

object Test extends GraphBuilder(TestDefinition.mapping, "Test") {
	defn a in ("x") -> out ("y")
	defn b in ("x", "t") -> out ("y")
	defn c in ("y", "p") -> out ("f") 
}

object Main {
  def main(args: Array[String]): Unit = {
  	println(Test get)
  	GV create (Test get) draw
  }
}
