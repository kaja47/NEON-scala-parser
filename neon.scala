/*
 * Author: Karel Čížek kaja@k47.cz funkcionalne.cz
 * License: new BSD
 */

import scala.util.parsing.combinator.RegexParsers

sealed trait Value extends AssocValue
sealed trait AssocValue
trait Scalar extends Value

case class Str(value: String)                 extends Scalar
case class Bool(value: Boolean)               extends Scalar
case class Integer(value: Long)               extends Scalar
case class FloatingPoint(value: Double)       extends Scalar
case class Assoc(seq: Seq[AssocValue])        extends Value
case class Entity(name: String, attrs: Assoc) extends Value
case class Pair(k: Value, v: Value)           extends AssocValue

object Scalar {
  val trueregex  = "[Tt]rue|[Oo]n|[Yy]es|TRUE|ON|YES".r
  val falseregex = "[Ff]alse|[Oo]ff|[Nn]o|FALSE|OFF|NO".r
  case class Conv[T](f: String => T) {
    def unapply(s: String) = try { Some(f(s)) } catch { case e: NumberFormatException => None }
  }
  val float = Conv(_.toDouble)
  val long  = Conv(_.toLong)

  def apply(str: String): Scalar = str match {
    case trueregex()  => Bool(true)
    case falseregex() => Bool(false)
    case long(l)      => Integer(l)
    case float(f)     => FloatingPoint(f)
    case str          => Str(str)
  }
}


object Neon extends RegexParsers {

  override protected val whiteSpace = """([\t ]*(#[^\n]*)?)+""".r // stripping spaces from last line

  lazy val indent:          Parser[String] = """(\n[\t ]*(#[^\n]*)?)+""".r ^^ { str => str.substring(str.lastIndexOf('\n')) }
  lazy val str:             Parser[String] = """"(\\.|[^"\n\\])*\"|'[^'\n]*'""".r
  lazy val litBoolIntFloat: Parser[String] = """[^#\"\',=\[\]{}\(\)\x00-\x20!`](?:[^,:=\]}\)\(\x00-\x20]+|:(?![\s,\]})]|$)|[ \t]+[^#,:=\]}\)\(\x00-\x20])*""".r
  lazy val id:              Parser[String] = litBoolIntFloat

  def scalar:               Parser[Scalar] = (str | litBoolIntFloat) ^^ Scalar.apply
  def oneline:              Parser[Value]  = list | entity | scalar

  private def indentAs(ind: Int)   = indent flatMap { indStr => if (indStr.length == ind) success(())            else failure("wrong indentation") }
  private def indentMore(ind: Int) = indent flatMap { indStr => if (indStr.length >  ind) success(indStr.length) else failure("wrong indentation") }

  def indentKeyVal(ind: Int): Parser[AssocValue] = (id <~ ":") ~ (indentListSeq(ind) | indentKeyValSeq(ind) | oneline) ^^ { case k ~ v => Pair(Scalar(k), v) }
  def indentKeyValSeq(ind: Int): Parser[Assoc] = indentSeq(ind, indentKeyVal)

  def indentList(ind: Int): Parser[AssocValue] =          "-" ~> (indentListSeq(ind) | indentKeyValSeq(ind) | oneline)
  def indentListSeq(ind: Int): Parser[Assoc] = indentSeq(ind, indentList)

  def indentSeq(ind: Int, p: Int => Parser[AssocValue]) =
    indentMore(ind) >> { ind => p(ind) ~ rep(indentAs(ind) ~> p(ind)) } ^^ { case v ~ vs => Assoc(v +: vs) }

  def list: Parser[Assoc] = ("{" | "[" | "(") >> { paren => repsep(kvPair, ",") <~ closing(paren) } ^^ Assoc
  private val closing = Map("{" -> "}", "[" -> "]", "(" -> ")")
  def kvPair = (id <~ (":" | "=")).? ~ oneline ^^ { case k ~ v => if (k.nonEmpty) Pair(Scalar(k.get), v) else v }

  def entity: Parser[Entity] = id ~ list ^^ { case id ~ kvs => Entity(id, kvs) }

  def base: Parser[Assoc] = (indentMore(-1).?.map(_ getOrElse 0) >> { ind => indentList(ind) | indentKeyVal(ind) }).* <~ indent.? ^^ Assoc

  def apply(str: String) = parseAll(base, str)
}


//def prettyPrint(value: Value, offset: Int = 0): Unit = {
//  def print(str: String) = println(" "*offset + str)
//  value match {
//    case s: Scalar => print("Scalar: "+s)
//    case Assoc(v) =>
//      print("Assoc:")
//      v foreach {
//        case Pair(k, v) => prettyPrint(k, offset); prettyPrint(v, offset + 2)
//        case x: Value => prettyPrint(x, offset + 2)
//      }
//    case Entity(name, attrs) =>
//      print("Entity: "+name)
//      prettyPrint(attrs, offset + 2)
//  }
//}
