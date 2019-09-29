package scalisp

import scala.util.parsing.combinator._
import scalisp.tokens._

trait ScalispInterpreterError
case class ScalispParserError(msg : String) extends ScalispInterpreterError

class ScalispParser extends RegexParsers {

  def symbol: Parser[SYMBOL] = {"""\w+""".r ^^ { str => SYMBOL(str) }}

  def float: Parser[FLOAT] = { """\d.\d+""".r ^^ { float => FLOAT(float) }}

  def int: Parser[INT] = {"""\d""".r ^^ { int => INT(int) }}

  def div: Parser[DIV.type] = {
    "/" ^^ { _ => DIV }
  }

  def mult: Parser[MULT.type] = {
    "*" ^^ { _ => MULT }
  }

  def plus: Parser[PLUS.type] = {
    "+" ^^ { _ => PLUS }
  }

  def minus: Parser[MINUS.type] = {
    "-" ^^ { _ => MINUS }
  }

  def gt: Parser[GT.type] = {
    ">" ^^ { _ => GT }
  }

  def lt: Parser[LT.type] = {
    "<" ^^ { _ => LT }
  }

  def gte: Parser[GTE.type] = {
    ">=" ^^ { _ => GTE }
  }

  def lte: Parser[LTE.type] = {
    "<=" ^^ { _ => LTE }
  }

  def quote: Parser[QUOTE.type] = {
    "quote " ^^ { _ => QUOTE }
  }

  def car: Parser[CAR.type] = {
    "car " ^^ { _ => CAR }
  }

  def cdr: Parser[CDR.type] = {
    "cdr " ^^ { _ => CDR }
  }

  def cons: Parser[CONS.type] = {
    "cons " ^^ { _ => CONS }
  }

  def define: Parser[DEFINE.type] = {
    "define " ^^ { _ => DEFINE }
  }

  def set: Parser[SET.type] = {
    "set! " ^^ { _ => SET }
  }

  def lambda: Parser[LAMBDA.type] = {
    "lambda " ^^ { _ => LAMBDA }
  }

  def nil: Parser[NULL.type] = {
    "null " ^^ { _ => NULL }
  }

  def atom: Parser[ATOM.type] = {
    "atom? " ^^ { _ => ATOM }
  }

  def eq: Parser[EQ.type] = {
    "eq? " ^^ { _ => EQ }
  }

  def begin: Parser[BEGIN.type] = {
    "begin " ^^ { _ => BEGIN }
  }

  def loop: Parser[LOOP.type] = {
    "loop " ^^ { _ => LOOP }
  }

  def if_inter: Parser[IF.type] = {
    "if " ^^ { _ => IF }
  }

  def sexp: Parser[SEXP] = {
    "(" ~> rep(sexp_tok) <~ ")" ^^ { list => SEXP(list) }
  }

  def sexp_tok : Parser[ScalispToken] = div | mult | plus | minus | gte | lte | gt | lt | quote | car | cdr | cons |
    define | set | lambda | nil | atom | eq | begin | loop | if_inter | float | int | symbol | sexp

  def apply(code: String): ParseResult[SEXP] = {
    parse(sexp, code)
  }

}

object Test extends App{
  override def main(args : Array[String]): Unit ={
    val parser = new ScalispParser()
    for ( x <- Seq(
      "(car (+ 4 (cons (3) (3 4))))",
      "(car (+ 4 (cons (3) ( * 3 4))))",
      "(set! x (+ 4 (cons (3) (3 4))))",
      "(set! x (+ 4 (cons (3) (3 4))"
      )){
      var t = parser(x)
      try{
        println(t.get)
      }
      catch {
        case re : RuntimeException => println(t)
      }

    }

  }
}
