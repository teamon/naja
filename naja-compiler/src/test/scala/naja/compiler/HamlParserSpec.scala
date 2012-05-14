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

    "Parse empty signature" in {
      check("""@()""", Template("()", Nil))
    }

    "Parse simple signature" in {
      check("""@(a: Int)""", Template("(a: Int)", Nil))
    }

    "Parse more complicated signature" in {
      check("""
        |@(a: Int, b: String)(implicit ev: Ev[Int])
        |
        |%html
      """, Template("(a: Int, b: String)(implicit ev: Ev[Int])", Node(Tag(Some("html"))) :: Nil))
    }
  }

  "Basic HAML" should {
    "literal text" in {
      checkBody("""some text""", Node(Text(LiteralText("some text") :: Nil)) :: Nil)

      checkBody("""
        |some text
        |on multiple
        |lines
      """,
        Node(Text(LiteralText("some text") :: Nil)) ::
        Node(Text(LiteralText("on multiple") :: Nil)) ::
        Node(Text(LiteralText("lines") :: Nil)) ::
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
      checkBody("""%span(a="1" b="2")""", Node(Tag(Some("span"), attributes = Map("a" -> Text(LiteralText("1") :: Nil), "b" -> Text(LiteralText("2") :: Nil)))) :: Nil)
    }

    "autoclosed tag" in {
      checkBody("""%img/""", Node(Tag(Some("img"), autoclose = true)) :: Nil)
      checkBody("""%img""", Node(Tag(Some("img"), autoclose = false)) :: Nil)
    }

    "tag with content" in {
      checkBody("""%span Some content""", Node(Tag(Some("span"), content = Some(Text(LiteralText("Some content") :: Nil)))) :: Nil)
    }
  }

  "interpolated text" in {
    checkBody(
      """One plus two is #{1+2} yea! #{x} really""",
      Node(Text(
        LiteralText("One plus two is ") ::
        InterpolatedText("1+2") ::
        LiteralText(" yea! ") ::
        InterpolatedText("x") ::
        LiteralText("really") ::
        Nil)
      ) :: Nil
    )
  }.pendingUntilFixed

  "print scala evaluated statement (= )" should {
    "very simple statement" in {
      checkBody("""= 1 + 2""", Node(Evaluated("1 + 2")) :: Nil)
    }
  }
}




