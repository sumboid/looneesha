object Main {
  def main(args: Array[String]): Unit = {
    val runtime = LRuntime("input.ln")

    runtime.writeData
    runtime.writeSolution
    runtime.visualize
  }
}
