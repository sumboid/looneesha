import java.io.FileReader
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

class SpecParser extends RegexParsers {
  def program: Parser[Graph] = rep(cf) ^^ { Graph }
  def cf: Parser[CF] = name~"->"~in~"->"~out~";" ^^ { case name~"->"~in~"->"~out~";" => CF(name, in, out) }
  def df: Parser[DF] = name ^^ { DF(_) }
  def in: Parser[List[DF]] = rep(df)
  def out: Parser[List[DF]] = rep(df)
  def name: Parser[String] = "[a-zA-Z_0-9]\\w*".r
}

object SpecParser extends SpecParser {
  def parse(source: String) = parseAll(program, source).get
  def parse(source: FileReader) = parseAll(program, source).get
}

trait InputParser extends JavaTokenParsers {
  def assign: Parser[DF] = name~"="~floatingPointNumber ^^ {
    case name~"="~number => DF(name, true)
  }
  def question: Parser[DF] = name~"?" ^^ { case name~"?" => DF(name) }
  def include: Parser[String] = "import"~stringLiteral ^^ { case "import"~n => n.toList.filter(_ != '"').mkString }
  def name: Parser[String] = "[a-zA-Z_0-9]\\w*".r
}

class IncludeParser extends InputParser {
  def program = rep(_program)
  def _program = (assign | include | question) ^^ { case inc : String => Some(inc); case _ => None }
}

object IncludeParser extends IncludeParser {
  def parse(source: String) = parseAll(program, source).get.filterNot(_.isEmpty).map(_.get)
}

class AssignParser extends InputParser {
  def program = rep(_program)
  def _program = (assign | include | question) ^^ { case df: DF => if (df.define) Some(df) else None; case _ => None }
}

object AssignParser extends AssignParser {
  def parse(source: String) = parseAll(program, source).get.filterNot(_.isEmpty).map(_.get)
}


class QuestionParser extends InputParser {
  def program = rep(_program)
  def _program = (assign | include | question) ^^ { case df: DF => if (df.define == false) Some(df) else None; case _ => None }
}

object QuestionParser extends QuestionParser {
  def parse(source: String) = parseAll(program, source).get.filterNot(_.isEmpty).map(_.get)
}