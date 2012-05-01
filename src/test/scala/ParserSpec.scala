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
      check("""@(a: Int, b: String)(implicit ev: Ev[Int])""", Template("(a: Int, b: String)(implicit ev: Ev[Int])", Nil))
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
      checkBody("""%html""", Node(Tag("html", Nil, Map.empty)) :: Nil)

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
        Node(Tag("html", Nil, Map.empty),
          Node(Tag("head", Nil, Map.empty),
            Node(Tag("ul", Nil, Map.empty),
              Node(Tag("li", Nil, Map.empty)) ::
              Node(Tag("li", Nil, Map.empty)) ::
              Nil
            ) ::
            Node(Tag("div", Nil, Map.empty)) ::
            Nil
          ) ::
          Node(Tag("body", Nil, Map.empty),
            Node(Tag("div", Nil, Map.empty),
              Node(Tag("span", Nil, Map.empty),
                Node(Tag("strong", Nil, Map.empty)) ::
                Nil
              ) ::
              Nil
            ) ::
            Node(Tag("table", Nil, Map.empty)) ::
            Nil
          ) ::
          Nil
        ) ::
        Nil
      )
    }

    "html class (.class)" in {
      checkBody("""%span.ca""", Node(Tag("span", "ca" :: Nil, Map.empty)) :: Nil)
      checkBody("""%span.ca.cb.cc""", Node(Tag("span", "ca" :: "cb" :: "cc" :: Nil, Map.empty)) :: Nil)
    }

    "html id (#id)" in {
      checkBody("""%span#my-span""", Node(Tag("span", Nil, Map("id" -> LiteralText("my-span")))) :: Nil)
    }

    "html attributes" in {
      checkBody("""%span(a="1" b="2")""", Node(Tag("span", Nil, Map("a" -> LiteralText("1"), "b" -> LiteralText("2")))) :: Nil)
    }
  }
}
