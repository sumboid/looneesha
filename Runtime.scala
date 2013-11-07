package looneesha

import scala.actors.Actor 

class Runtime(graph: Graph, input: List[DF], output: List[DF]) extends Actor {
	var actors: List[Actor] = Nil

  def visualize = GV.create(graph, Graph(graph.paths(input, output).flatMap(_.cfs)), input, output).draw
  def writeData = { 
  	println("Graph: " + graph)
  	println("Input: " + input)
  	println("Output: " + output)
  }
  def writeSolution = println("Paths: " + graph.paths(input, output))

 	def act = {
 		for(i <- 1 to output.size) {
 			react {
 				case df: DF => println("Look! " + df.name + " = " + df.value)
 				case _ => println("ugh?") 
 			}
 		}
 	}

 	override def start: Unit = {
 		super.start
 		val paths = graph.paths(input, output)
 		paths foreach initComputation
 	}

 	def initComputation(g: Graph) = {
 		
 		def getlink(dfs: List[DF]) = g.filterIn(dfs) match {
 				case Nil => dfs map (df => df -> this)
 				case cfs => dfs flatMap (df => cfs filter (cf => cf.in contains df) map (cf => df -> cf)) 
 			}

 		def _initComputation(cfs: List[CF]) = {
 			cfs.foreach(actors ::= CFRuntime(_, getlink(_.out))) 
 			_initComputation(g.filterOut(cfs.in))
 		}

 		_initComputation(g.filterOut(out :: Nil))
 	}


}

object Runtime {
	def apply(g: GraphBuilder, p: ProblemBuilder) = new Runtime(g.get, p.dfs, p.question)
}

case class CFRuntime(cf: CF, link: List[(DF, Actor)]) extends Actor {
	def act = {
		var input: List[DF] = cf.input
		for(i <- 1 to input.size) {
			react {
				case df: DF => {
					val index = input.indexOf(df)
					input(index) = input(index).set(df.value)
				}
			}
		}
		val realcf = cf.set(input)
		val result = realcf.run
		result foreach (df => link filter (l => l._1 == df) foreach (l => l._2 ! df))
	}
}