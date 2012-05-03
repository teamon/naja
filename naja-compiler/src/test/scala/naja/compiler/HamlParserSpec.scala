package naja.compiler

import org.specs2.mutable._

class ParserSpec extends Specification {
  import HamlParser._

  def check[A](s: String, a: A) = HamlParser(s.stripMargin) match {
    case Success(tpl, _) => tpl === a
    case Failure(msg, _)  => failure(msg)
  }

  def checkBody[A](s: String, a: A) = HamlParser(s.stripMargin) match {
    case Success(tpl, _) => tpl.body === a
    case Failure(msg, _)  => failure(msg)
  }

  "Function signature" should {
    "Provide empty default" in {
      check("""""", Template("()", Nil))
    }

    "Parse @signature" in {
      check("""@()""", Template("()", Nil))
      check("""@(a: Int)""", Template("(a: Int)", Nil))
      check("""
        |@(a: Int, b: String)(implicit ev: Ev[Int])
        |
        |%html
      """, Template("(a: Int, b: String)(implicit ev: Ev[Int])", Node(Tag("html")) :: Nil))
    }
  }

  "Basic HAML" should {
    "literal text" in {
      checkBody("""some text""", Node(LiteralText("some text")) :: Nil)

      checkBody("""
        |some text
        |on multiple
        |lines
      """,
        Node(LiteralText("some text")) ::
        Node(LiteralText("on multiple")) ::
        Node(LiteralText("lines")) ::
        Nil
      )
    }

    "basic html tags" in {
      checkBody("""%html""", Node(Tag("html")) :: Nil)

      checkBody("""
        |%html
        |  %head
        |    %ul
        |      %li
        |      %li
        |
        |    %div
        |  %body
        |    %div
        |      %span
        |        %strong
        |
        |    %table
      """,
        Node(Tag("html"),
          Node(Tag("head"),
            Node(Tag("ul"),
              Node(Tag("li")) ::
              Node(Tag("li")) ::
              Nil
            ) ::
            Node(Tag("div")) ::
            Nil
          ) ::
          Node(Tag("body"),
            Node(Tag("div"),
              Node(Tag("span"),
                Node(Tag("strong")) ::
                Nil
              ) ::
              Nil
            ) ::
            Node(Tag("table")) ::
            Nil
          ) ::
          Nil
        ) ::
        Nil
      )
    }

    "html class (.class)" in {
      checkBody("""%span.ca""", Node(Tag("span", "ca" :: Nil)) :: Nil)
      checkBody("""%span.ca.cb.cc""", Node(Tag("span", "ca" :: "cb" :: "cc" :: Nil)) :: Nil)
    }

    "html id (#id)" in {
      checkBody("""%span#my-span""", Node(Tag("span", Nil, Map("id" -> LiteralText("my-span")))) :: Nil)
    }

    "html attributes" in {
      checkBody("""%span(a="1" b="2")""", Node(Tag("span", Nil, Map("a" -> LiteralText("1"), "b" -> LiteralText("2")))) :: Nil)
    }

    "autoclosed tag" in {
      checkBody("""%img/""", Node(Tag("img", autoclose = true)) :: Nil)
      checkBody("""%img""", Node(Tag("img", autoclose = false)) :: Nil)
    }
  }
}
