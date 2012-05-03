package naja.compiler

import scala.util.parsing.input.Positional

sealed trait Expr extends Positional
case class LiteralText(content: String) extends Expr
case class Tag(name: Option[String],
               id: Option[String] = None,
               classes: List[String] = Nil,
               attributes: Map[String, Expr] = Map.empty,
               autoclose: Boolean = false,
               body: List[Expr] = Nil) extends Expr

case class Template(signature: String, body: List[Node])
case class Node(content: Expr, body: List[Node] = Nil)
