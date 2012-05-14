package naja.compiler

import scala.annotation.tailrec
import scala.util.parsing.combinator._
import scala.util.parsing.input._

trait IndentedParser extends RegexParsers {
  override def skipWhitespace = false

  def indentUnit = """  """.r

  def nl = """\r*\n""".r

  def nestedBlocks(element: Parser[Expr]) = {
    def line(n: Int): Parser[Node] = repN(n, indentUnit) ~ (element /*| err("Inconsistent indentation")*/) ~ nl ~ rep(line(n+1)) ^^ {
      case in ~ s ~ ne ~ body => Node(s, body)
    }
    rep(line(0))
  }

  def addMissingNl(s: String) = if(s != "" && s.last == '\n') s else s + '\n'
  def removeEmptyLines(s: String) = s.split("\n").filterNot(_ matches """\s*""").mkString("\n")
}

object HamlParser extends IndentedParser {
  def signature = "@" ~> ".+".r <~ nl
  def validName = """[A-Za-z0-9_-]+""".r

  def interpolatedText = """#{""" ~> """[^}]+""".r <~ """}""" ^^ InterpolatedText
  def literalText = ("""(\\#)[^#]*""".r | """[^#\n\r]+""".r) ^^ { s =>
    val a = if(s.head == '\\') s.drop(1) else s // TODO: This is kind of hack
    LiteralText(a)
  }

  def text = rep1(interpolatedText | literalText) ^^ Text

  def tagName = "%" ~> validName
  def tagId = "#" ~> validName
  def tagClass = "." ~> validName
  def tagClasses = rep(tagClass)
  def tagAttributesKey = validName
  def tagAttributesValue = "\"" ~> /*literalText*/validName <~ "\"" ^^ { s => Text(LiteralText(s) :: Nil) } // TODO!
  def tagAttributes = "(" ~> rep(tagAttributesKey ~ ("=" ~> tagAttributesValue) <~ " *".r) <~ ")"

  def tagStartingWithName: Parser[(Option[String], Option[String], List[String])] = (tagName ~ tagClasses ~ opt(tagId) ~ tagClasses) ^^ {
    case name ~ classes1 ~ idOpt ~ classes2 => (Some(name), idOpt, classes1 ::: classes2)
  }
  def tagStartingWithId: Parser[(Option[String], Option[String], List[String])] = (tagId ~ tagClasses) ^^ {
    case id ~ classes => (None, Some(id), classes)
  }
  def tagStaringWithClass: Parser[(Option[String], Option[String], List[String])] = (tagClass ~ tagClasses ~ opt(tagId) ~ tagClasses) ^^ {
    case cls ~ classes1 ~ idOpt ~ classes2 => (None, idOpt, cls :: classes1 ::: classes2)
  }
  def tagBegining = (tagStartingWithName | tagStartingWithId | tagStaringWithClass)

  def tag = tagBegining ~ opt(tagAttributes) ~ opt("/".r) ~ (" *".r ~> opt(notTag)) ^^ {
    case (nameOpt, idOpt, classes) ~ attributesOpt ~ autoclose ~ contentOpt =>
      val attrs = attributesOpt.map {
        _.foldLeft(Map[String, Expr]()) { case (map, key ~ value) => map + (key -> value) }
      } getOrElse Map()

      Tag(nameOpt, idOpt, classes, attrs, autoclose.isDefined, contentOpt)
  }

  def scalaExpression = """ *[^\n]+""".r

  def evaluated = "=" ~> scalaExpression ^^ { e => Evaluated(e.dropWhile(' '==)) }
  def statement = "-" ~> scalaExpression ^^ { e => Statement(e.dropWhile(' '==)) }

  def notTag = positioned(evaluated | statement | text)
  def element = positioned(tag | notTag)

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
