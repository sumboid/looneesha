
/*
 * Get input file with imports, assigns and questions
 * Parse imports, create graphs
 * Parse assigns and questions, then time to execute program: find paths and start async execution.
 */

import java.io.FileReader

class LRuntime(graph: Graph, input: List[DF], output: List[DF]) {
  def visualize = GV.create(graph).draw
  //def visualize(path: Graph) = GV.create(graph, path).draw
}

object LRuntime {
	def apply(file: String) = {
		val input = scala.io.Source.fromFile(file).getLines.mkString
		val specfiles = IncludeParser.parse(input)
		val in = AssignParser.parse(input)
		val out = QuestionParser.parse(input)

		val graphs = specfiles.map(file => SpecParser.parse(new FileReader(file)))
		val graph = Graph(graphs.flatMap(g => g.cfs))

		new LRuntime(graph, in, out)
	}
}
