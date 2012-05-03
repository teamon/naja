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
      """, Template("(a: Int, b: String)(implicit ev: Ev[Int])", Node(Tag(Some("html"))) :: Nil))
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
      checkBody("""%html""", Node(Tag(Some("html"))) :: Nil)

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
        Node(Tag(Some("html")),
          Node(Tag(Some("head")),
            Node(Tag(Some("ul")),
              Node(Tag(Some("li")))::
              Node(Tag(Some("li")))::
              Nil
            ) ::
            Node(Tag(Some("div")))::
            Nil
          ) ::
          Node(Tag(Some("body")),
            Node(Tag(Some("div")),
              Node(Tag(Some("span")),
                Node(Tag(Some("strong")))::
                Nil
              ) ::
              Nil
            ) ::
            Node(Tag(Some("table")) )::
            Nil
          ) ::
          Nil
        ) ::
        Nil
      )
    }

    "html class (.class)" in {
      checkBody(""".ca""", Node(Tag(None, None, "ca" :: Nil)) :: Nil)
      checkBody("""%span.ca""", Node(Tag(Some("span"), None, "ca" :: Nil)) :: Nil)
      checkBody("""%span.ca.cb.cc""", Node(Tag(Some("span"), None, "ca" :: "cb" :: "cc" :: Nil)) :: Nil)
    }

    "html id (#id)" in {
      checkBody("""#my-span""", Node(Tag(None, Some("my-span"))) :: Nil)
      checkBody("""%span#my-span""", Node(Tag(Some("span"), Some("my-span"))) :: Nil)
    }

    "html mixed id and class" in {
      checkBody("""%span.a#my.b.c""", Node(Tag(Some("span"), Some("my"), "a" :: "b" :: "c" :: Nil)) :: Nil)
      checkBody(""".a#my.b.c""", Node(Tag(None, Some("my"), "a" :: "b" :: "c" :: Nil)) :: Nil)
      checkBody(""".a.b#my.c""", Node(Tag(None, Some("my"), "a" :: "b" :: "c" :: Nil)) :: Nil)
      checkBody("""#my.a.b.c""", Node(Tag(None, Some("my"), "a" :: "b" :: "c" :: Nil)) :: Nil)
    }


    "html attributes" in {
      checkBody("""%span(a="1" b="2")""", Node(Tag(Some("span"), attributes = Map("a" -> LiteralText("1"), "b" -> LiteralText("2")))) :: Nil)
    }

    "autoclosed tag" in {
      checkBody("""%img/""", Node(Tag(Some("img"), autoclose = true)) :: Nil)
      checkBody("""%img""", Node(Tag(Some("img"), autoclose = false)) :: Nil)
    }
  }
}
