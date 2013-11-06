package looneesha

/*
 * Get input file with imports, assigns and questions
 * Parse imports, create graphs
 * Parse assigns and questions, then time to execute program: find paths and start async execution.
 */

import java.io.FileReader
import akka.actor.Actor
import java.lang.reflect.Method
import java.lang.Class

class LRuntime(graph: Graph, input: List[DF], output: List[DF]) extends Actor {
  def visualize = GV.create(graph, Graph(graph.paths(input, output).flatMap(_.cfs)), input, output).draw
  def writeData = { 
  	println("Graph: " + graph)
  	println("Input: " + input)
  	println("Output: " + output)
  }
  def writeSolution = println("Paths: " + graph.paths(input, output))

 	def receive = {
 		case _ => println("aha")
 	}

 	def getCF(c: String, m: String) = Class.forName(c).getMethod(m)
}

object LRuntime {
	def apply(file: String) = {
		val input = scala.io.Source.fromFile(file).getLines.mkString
		val specfiles = IncludeParser.parse(input)
		val in = AssignParser.parse(input)
		val out = QuestionParser.parse(input)

		val graphs = specfiles.map(file => SpecParser.parse(new FileReader(file)))
		val graph = Graph(graphs.flatMap(_.cfs))
		new LRuntime(graph, in, out)
	}
}

class CFRuntime(c: Class[_], m: Method, input: Seq[DF], output: Seq[DF],
							  link: List[(DF, CFRuntime)]) extends Actor {

	var realInput: List[DF] = Nil // Need fixed size list. 
	var realOutput: List[DF] = Nil 

	def start = ??? //check some

	def receive = {
		case df: DF => //set realInput, check input complete, and wait or invoke and send. 
	}

	def invokeMethod = { realOutput = m.invoke(c, realInput).asInstanceOf[List[DF]] }
	def sendAll = realOutput foreach (df => link filter (l => l._1 == df) map (l => l._2) foreach (cfr => cfr receive df))
}