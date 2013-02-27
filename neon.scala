/*
 * Author: Karel Čížek kaja@k47.cz funkcionalne.cz
 * License: new BSD
 */

import scala.util.parsing.combinator.RegexParsers

sealed trait Value extends AssocValue {
  def simplify: Any
}
sealed trait AssocValue
trait Scalar extends Value {
  def value: Any
  def simplify = value
}

case class Str(value: String)                 extends Scalar
case class Bool(value: Boolean)               extends Scalar
case class Integer(value: Long)               extends Scalar
case class FloatingPoint(value: Double)       extends Scalar
case object Null                              extends Scalar { def value = null }
case class Assoc(seq: Seq[AssocValue])        extends Value {
  def simplify =
    if (seq forall (_.isInstanceOf[Pair])) {
      seq collect { case Pair(k, v) => (k.simplify, v.simplify) } toMap
    } else if (seq forall (_.isInstanceOf[Value])) {
      seq collect { case x: Value => x.simplify }
    } else {
      sys.error("mixed Assocs cannot be simplified")
    }
}
case class Entity(name: String, attrs: Assoc) extends Value {
  def simplify = (name, attrs.simplify)
}
case class Pair(k: Value, v: Value)           extends AssocValue

object Scalar {
  val trueregex  = "[Tt]rue|[Oo]n|[Yy]es|TRUE|ON|YES".r
  val falseregex = "[Ff]alse|[Oo]ff|[Nn]o|FALSE|OFF|NO".r
  val nullregex  = "[Nn]ull|NULL".r
  case class Conv[T](f: String => T) {
    def unapply(s: String) = try { Some(f(s)) } catch { case e: NumberFormatException => None }
  }
  val float = Conv(_.toDouble)
  val long  = Conv(_.toLong)

  def apply(str: String): Scalar = str match {
    case trueregex()  => Bool(true)
    case falseregex() => Bool(false)
    case nullregex()  => Null
    case long(l)      => Integer(l)
    case float(f)     => FloatingPoint(f)
    case str          => Str(str)
  }
}


object Neon extends RegexParsers {

  override protected val whiteSpace = """([\t ]*(#[^\n]*)?)+""".r // stripping spaces or comments from last line

  lazy val indent: Parser[String] = """(\n[\t ]*(#[^\n]*)?)+""".r ^^ { str => str.substring(str.lastIndexOf('\n')) }
  lazy val str1:   Parser[String] = """"(\\.|[^"\n\\])*"""".r      ^^ { str => str.substring(1, str.length-1).replaceAll("""\\([\\"])""", "$1").replaceAll("""\\n""", "\n").replaceAll("""\\t""", "\t") }
  lazy val str2:   Parser[String] = """'[^'\n]*'""".r              ^^ { str => str.substring(1, str.length-1) }
  lazy val value:  Parser[String] = """[^#\"\',=\[\]{}\(\)\x00-\x20!`](?:[^,:=\]}\)\(\x00-\x20]+|:(?![\s,\]})]|$)|[ \t]+[^#,:=\]}\)\(\x00-\x20])*""".r // literal / bool / int / float

  lazy val scalar:               Parser[Scalar] = (str1 | str2 | value) ^^ Scalar.apply
  def oneline:              Parser[Value]  = list | entity | scalar

  private def indentAs(ind: Int)   = indent flatMap { indStr => if (indStr.length == ind) success(())            else failure("wrong indentation") }
  private def indentMore(ind: Int) = indent flatMap { indStr => if (indStr.length >  ind) success(indStr.length) else failure("wrong indentation") }

  def indentKeyVal(ind: Int): Parser[AssocValue] = (value <~ ":") ~ opt(indentListSeq(ind) | indentKeyValSeq(ind) | oneline) ^^ { case k ~ v => Pair(Scalar(k), v.getOrElse(Null)) }
  def indentKeyValSeq(ind: Int): Parser[Assoc] = indentSeq(ind, indentKeyVal)

  def indentList(ind: Int): Parser[AssocValue] =          "-" ~> (indentListSeq(ind) | indentKeyValSeq(ind) | oneline)
  def indentListSeq(ind: Int): Parser[Assoc] = indentSeq(ind, indentList)

  def indentSeq(ind: Int, p: Int => Parser[AssocValue]) =
    indentMore(ind) >> { ind => p(ind) ~ rep(indentAs(ind) ~> p(ind)) } ^^ { case v ~ vs => Assoc(v +: vs) }

  def list: Parser[Assoc] = ("{" | "[" | "(") >> { paren => indent.? ~> repsep(kvPair, "," ~ indent.? | indent) <~ indent.? <~ closing(paren) } ^^ Assoc
  private val closing = Map("{" -> "}", "[" -> "]", "(" -> ")")
  def kvPair = opt(value <~ (":" | "=")) ~ oneline ^^ { case k ~ v => if (k.nonEmpty) Pair(Scalar(k.get), v) else v }

  def entity: Parser[Entity] = value ~ list ^^ { case id ~ kvs => Entity(id, kvs) }

  def base: Parser[Assoc] = (indentMore(-1).?.map(_ getOrElse 0) >> { ind => indentList(ind) | indentKeyVal(ind) | oneline }).* <~ indent.? ^^ Assoc

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
