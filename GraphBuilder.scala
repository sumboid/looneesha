package looneesha

import scala.language.dynamics
import scala.language.postfixOps
import scala.language.implicitConversions

trait GraphBuilder {
	var cfs: List[CF] = Nil

	object defn extends Dynamic {
		def applyDynamic(m: String)(args: => (List[DF], List[DF])) = { 
			val (in, out) = args 
			cfs ::= CF(m, in, out)
		} 
	}

	object in extends Dynamic {
		def applyDynamic(f: String)(args: String*) = { args.toList map (DF(_)) }
	}

	object out extends Dynamic {
		def applyDynamic(f: String)(args: String*) = { args.toList map (DF(_)) }
	}

	def get = Graph(cfs)
}