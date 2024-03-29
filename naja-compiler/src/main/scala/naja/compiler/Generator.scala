package naja.compiler

import treehugger.forest._
// import definitions._
import treehugger.forest.treehuggerDSL._

object Generator {
  case class Config(
    defaultTagName: String
  )

  val defaultConfig = Config(
    defaultTagName = "div"
  )

  def apply(template: Template, config: Config = defaultConfig) = {
    val gen = new Generator(config)
    gen(template)
  }
}

class Generator(config: Generator.Config){
  def apply(template: Template) = {
    val tree = sumTree(template.body.flatMap(e => generate(translate(e))))

    val res = """
    |// Source generated by naja
    |
    |object tpl {
    |  def apply%s = {
    |%s
    |  }
    |}
    """.stripMargin.format(template.signature, treeToString(tree))
    res
  }

  def sumTree(list: List[Tree]) = if(list.isEmpty) LIT("") else list.reduceLeft(_ INFIX("+") APPLY _)

  def translate(node: Node): Expr = node.content match {
    case el @ Tag(_, _, _, _, _, _, _) => el.copy(body = node.body.map(translate))
    case el @ Statement(_, _) => el.copy(body = node.body.map(translate))
    case el => el // TODO: Raise error if there is body present!
  }

  def bodyTree(body: List[Expr]) = if(body.isEmpty) Nil else BLOCK(sumTree(body.flatMap(generate))) :: Nil

  def generate(expr: Expr): List[Tree] = expr match {
    case Tag(name, id, classes, attrs, autoclose, content, body) =>
      // TODO: if(content.isDefined && !body.isEmpty) throw Exception("Tag can't have both content and body")
      // TODO: if(autoclose && (content.isDefined || !body.isEmpty)) throw Exception("Autoclosed tag can't have content nor body")

      val tagName = name getOrElse config.defaultTagName


      LIT("<" + tagName + ">") ::
      content.map(generate).getOrElse(Nil) :::
      bodyTree(body) :::
      LIT("</" + tagName + ">") ::
      Nil


      // openTag + closeTag


      // val attrs1 = id.map { v => Map("id" -> LiteralText(v)) } getOrElse Map()
      // val attrs2 = attrs1 ++ (attributesOpt.map {
      //   _.foldLeft(Map[String, Expr]()) { case (map, key ~ value) => map + (key -> value) }
      // } getOrElse Map())
      // val attrs3 = (attrs2.get("class").map(classes.::) getOrElse classes) match {
      //   case Nil => attrs2
      //   case xs => attrs2 + ("class" -> LiteralText(xs.mkString(" ")))
      // }
      // Tag(name, attrs1 ++ attrs2, autoclose.isDefined)

    case Evaluated(code) => LIT(code) :: Nil

    case Statement(code, body) => LIT(code) :: bodyTree(body)

    case LiteralText(text) => LIT(text) :: Nil
  }

  // protected def generateAttributes(attributes: Map[String, Expr]) = {
  //   attributes.mapValues(generate)
  // }
}

object TestRun {
  def main(args: Array[String]): Unit = {
    import HamlParser._

    val in = """
    |@(a: Int)
    |
    |%span#foo.bar
    |  %ul
    |    - val x = 4
    |    %li= a + 1
    |    - val y = "#"
    |    %li=a
    |    - if(x == 4)
    |      %li.red
    |    - else
    |      %li.blue
    |
    |  %ul.foo
    |    = List(1,2,3).map
    |      - (e)
    |        %li= e
    |
    |    - for(i <- (1 to 3))
    |      %li= i
    |
    """.stripMargin

    println(in)

    HamlParser(in) match {
      case Success(tpl, _) =>
        val source = Generator(tpl)
        println(source)

      case Failure(msg, _)  =>
        println(msg)
    }
  }
}
