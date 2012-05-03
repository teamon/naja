package naja.compiler

import scala.annotation.tailrec
import scala.util.parsing.combinator._
import scala.util.parsing.input._

trait IndentedParser extends RegexParsers {
  type E
  case class Node(content: E, body: List[Node] = Nil)

  override def skipWhitespace = false

  def indentUnit = """  """.r

  def nl = """\r*\n""".r

  def nestedBlocks(element: Parser[E]) = {
    def line(n: Int): Parser[Node] = repN(n, indentUnit) ~ (element /*| err("Inconsistent indentation")*/) ~ nl ~ rep(line(n+1)) ^^ {
      case in ~ s ~ ne ~ body => Node(s, body)
    }
    rep(line(0))
  }

  def addMissingNl(s: String) = if(s != "" && s.last == '\n') s else s + '\n'
  def removeEmptyLines(s: String) = s.split("\n").filterNot(_ matches """\s*""").mkString("\n")
}

object HamlParser extends IndentedParser {
  sealed trait Expr extends Positional
  type E = Expr

  case class Template(signature: String, body: List[Node])

  // case class Spaces(count: Int) extends Expr { override def toString = "Spaces(" + count + ", " + pos + ")"}
  // object NewLine extends Expr
  case class LiteralText(content: String) extends Expr
  case class Tag(name: String, classes: List[String] = Nil, attributes: Map[String, Expr] = Map.empty, autoclose: Boolean = false) extends Expr

  def signature = "@" ~> ".+".r <~ nl

  def literalText = """[\w ]+""".r ^^ { LiteralText(_) }

  def tagName = """%\w+""".r ^^ { _.drop(1) }
  def tagId = """#.+""".r ^^ { _.drop(1) }
  def tagClasses = rep("""\.\w+""".r)
  def tagAttributesKey = """\w+""".r
  def tagAttributesValue = "\"" ~> literalText <~ "\""
  def tagAttributes = "(" ~> rep(tagAttributesKey ~ ("=" ~> tagAttributesValue) <~ " *".r) <~ ")"
  def tag = tagName ~ opt(tagId) ~ tagClasses ~ opt(tagAttributes) ~ opt("/".r) ^^ {
    case name ~ id ~ classes ~ attributesOpt ~ autoclose =>
      val attrs1 = id.map { v => Map("id" -> LiteralText(v)) } getOrElse Map()
      val attrs2 = attributesOpt.map {
        _.foldLeft(Map[String, Expr]()) { case (map, key ~ value) => map + (key -> value) }
      } getOrElse Map()
      Tag(name, classes.map(_.drop(1)), attrs1 ++ attrs2, autoclose.isDefined)
  }

  def element = positioned(tag | literalText)

  def parser = opt(signature) ~ nestedBlocks(element) <~ rep(nl) ^^ {
    case sig ~ body => Template(sig getOrElse "()", body)
  }

  def apply(in: String) = {
    val input = addMissingNl(removeEmptyLines(in))
    phrase(parser)(new CharSequenceReader(input))
  }
}

//   def pp(s: Any){
//     def offset(lvl: Int) = (0 until lvl).map(e => "  ").mkString("")
//     val str = s.toString.foldLeft(("", 0, false)){
//       case ((xs, lvl, ind), '(') => (xs + "(\n", lvl+1, true)
//       case ((xs, lvl, ind), ')') => (xs + "\n" + offset(lvl-1)+")", lvl-1, true)
//       case ((xs, lvl, ind), ',') => (xs + ",\n", lvl, true)
//       case ((xs, lvl, ind), c) => (xs + (if(ind) offset(lvl) else "")+c, lvl, false)
//     }._1
//     println(str)
//   }
