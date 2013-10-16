object Main {
  def main(args: Array[String]): Unit = {
    val runtime = LRuntime("input.ln")
    //Test
    //val g = Graph(Nil)
    //println(g.combinations(List(CF("1", Nil, Nil) :: CF("2", Nil, Nil) :: CF("3", Nil, Nil) :: Nil, CF("4", Nil, Nil) :: CF("5", Nil, Nil) :: CF("6", Nil, Nil) :: Nil)))
    runtime.writeData
    runtime.writeSolution
    runtime.visualize
  }
}
