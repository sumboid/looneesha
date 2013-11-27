package looneesha
package translator

import java.io.FileReader
import scala.util.parsing.combinator.RegexParsers

case class Package (name: String) {
  override def toString = name
}

case class Func (in: List[Arg], out: List[Arg], body: Body) {
  override def toString = {
    "(" + { for (i <- 0 until in.size - 1) yield in(i) + ": Double, " }.mkString + in(in.size - 1) + ": Double" + ")" +
    " => {\n" + body + "}\n"
  }
}

case class Arg (name: String) {
  override def toString = name
}

case class Body (text: String) {
  override def toString = text
}

object GraphBuilderCreator {
  def argtostr(args: List[Arg]) = { for (i <- 0 until args.size - 1) yield "\"%s\", ".format(args(i)) }.mkString + "\"%s\""
  def apply(program: (Package, List[Func])) {
    var result = ""
    val (pack, funcs) = program
    println(pack); println(funcs)
    result += "object %s extends GraphBuilder(%sDef.mapping, \"%s\")".format(pack, pack, pack)
    result += "{\n"
    for(i <- 0 until funcs.size) {
      result += "defn func" + i + " " + "in(%s) -> out(%s)\n".format(argtostr(funcs(i).in), argtostr(funcs(i).out))
    }
    result += "}\n\n"
    result
  }
}

class Parser extends RegexParsers {
  def program: Parser[Map[Package, List[Func]]] = rep(pack) ^^ { Map() ++ _ }
  def pack: Parser[(Package, List[Func])] = "package"~"[a-zA-Z0-9_]\\w*".r~"{"~rep(func)~"}" ^^ { case "package"~name~"{"~funcs~"}" => Package(name) -> funcs }
  def func: Parser[Func] = "def"~in~"->"~out~"{"~body~"}" ^^ { case "def"~in~"->"~out~"{"~body~"}" => Func(in, out, body) }
  def in: Parser[List[Arg]] = rep(arg)
  def out: Parser[List[Arg]] = rep(arg)
  def arg: Parser[Arg] = "[a-zA-Z0-9_]\\w*".r ^^ { Arg }
  def body: Parser[Body] = subbody ^^ { case smth => Body(smth.mkString) }
  def subbody: Parser[String] = ( helper~"{"~subbody~"}"~subbody ^^ { case a~"{"~b~"}"~c => a + "{\n" + b + "}\n" + c }
                                | helper~"{"~subbody~"}" ^^ { case a~"{"~b~"}" => a + "{\n" + b + "}\n" }
                                | helper)
  def helper: Parser[String] = "[^{}]*".r 
}

object Parser extends Parser {
  def parse(source: String) = parseAll(program, source).get
  def parse(source: FileReader) = parseAll(program, source).get
}

object Main {
  def main (args: Array[String]): Unit = {
    val file = new FileReader(args(0))
    val program = Parser.parse(file)
    program foreach (pack => println(GraphBuilderCreator(pack)))
  } 
}