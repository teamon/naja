package naja.compiler

import scala.util.parsing.input.Positional

sealed trait Expr extends Positional
sealed trait TextExpr extends Expr

case class Text(content: List[TextExpr]) extends Expr
case class LiteralText(content: String) extends TextExpr
case class InterpolatedText(content: String) extends TextExpr

case class Tag(name: Option[String],
               id: Option[String] = None,
               classes: List[String] = Nil,
               attributes: Map[String, Expr] = Map.empty,
               autoclose: Boolean = false,
               content: Option[Expr] = None,
               body: List[Expr] = Nil) extends Expr

case class Evaluated(content: String) extends Expr
case class Statement(content: String, body: List[Expr] = Nil) extends Expr

case class Template(signature: String, body: List[Node])
case class Node(content: Expr, body: List[Node] = Nil)
