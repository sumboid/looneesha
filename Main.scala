package looneesha
import scala.collection.immutable.HashMap

object TestDefinition extends CFDefinition {
	val mapping = Map("a" -> ((in: List[DF]) => (in(0) + 1) :: Nil), 
										"b" -> ((in: List[DF]) => (in(0) + in(1)) :: Nil), 
										"c" -> ((in: List[DF]) => (in(0) - in(1)) :: Nil))
}

object Test extends GraphBuilder(TestDefinition.mapping, "Test") {
	defn a in ("x") -> out ("y")
	defn b in ("x", "t") -> out ("y")
	defn c in ("y", "p") -> out ("f") 
}

object Problem extends ProblemBuilder {
	defn Test_x 0
	defn Test_t 1
	defn Test_p 2
	quest Test_f
}
object Main {
  def main(args: Array[String]): Unit = {
  	val runtime = Runtime(Test, Problem)
  	runtime.init
    runtime.start
    //GV create (Test get) draw
  }
}
