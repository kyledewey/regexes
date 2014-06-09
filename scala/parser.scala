// Grammar:
// l in Letter
// r in Regex ::= . | l | r* | r_1 r_2

sealed trait Regex
case object AnyChar extends Regex
case class Letter(c: Char) extends Regex
case class Repeat(r: Regex) extends Regex
case class RegexAnd(r1: Regex, r2: Regex) extends Regex

// We could (and probably should) use standard parser combinators here,
// but I want to play with my own for now

object Parser {
  sealed trait ParseResult
  sealed trait Success extends ParseResult
  case object EmptySuccess extends Success
  case class NonemptySuccess(rest: String) extends Success
  case class Failure(error: String) extends ParseResult

  def failure(msg: String): Failure = Failure(msg)

  def success(rest: String): Success =
    if (rest == "") EmptySuccess else NonemptySuccess(rest)

  type P = String => ParseResult

  def literal(lit: String): P =
    (input: String) =>
      if (input.startsWith(lit)) 
        success(input.substring(lit.length))
      else
        failure("expected " + lit)

  def and(p1: => P, p2: => P): P =
    (input: String) =>
      p1(input) match {
        case NonemptySuccess(rest) => p2(rest)
        case other: ParseResult => other
      }

  def or(p1: => P, p2: => P): P =
    (input: String) =>
      p1(input) match {
        case EmptySuccess => EmptySuccess
        case _ => p2(input)
      }
} // Parser

object ParserTest {
  import Parser._

  // grammar:
  // r in Regex ::= . | a | r* | r_1 r_2

  lazy val test: P =
    or(literal("."),
       or(literal("a"),
          or(and(test, literal("*")),
             and(test, test))))
}
