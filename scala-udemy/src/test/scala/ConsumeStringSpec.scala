import ConsumeStringSpec.Token.{isPunctuation, isWhitespace}
import ConsumeStringSpec.{anonymise, tokenize, tokenize2}
import org.scalatest.funspec.AnyFunSpec

class ConsumeStringSpec extends AnyFunSpec {

  val tests =List(
    ("simple text", "foo bar 1 asdf4 12", List("foo", " ", "bar", " ", "1", " ", "asdf4", " ", "12")),
    ("text with characters", "foo, bar. 1 ; asdf4, 12;", List("foo,", " ","bar.", " ","1", " ",";", " ","asdf4,"," ", "12;")),
    ("text tabs", "foo\tasd", List("foo", "\t", "asd")),
    ("text consequtive spaces", " foo  asd", List(" ", "foo", " ", " ", "asd")),
    ("trailing space", " foo  ", List(" ", "foo", " ", " ")),
    ("text newlines", "foo \nasd\n1", List("foo", " ", "\n", "asd", "\n", "1")),
    ("text utf8 chars", "fóó \nąsd\n1", List("fóó", " ", "\n", "ąsd", "\n", "1")),
  )

  describe("tokenize") {
    tests.foreach { case (name, in, exp) => it(name) {
      assert(tokenize(in) == exp)
    }}
  }

  describe("tokenize2") {
    tests.foreach { case (name, in, exp) => it(name) {
      assert(tokenize2(in) == exp)
    }}
  }

  describe("anonymise") {
    def process(s: String): String = {
      val f = tokenize _ andThen anonymise
      f(s).mkString
    }

    it("x") {
      tests.map(_._2)
        .foreach(v => {
          println(v)
          println(process(v))
          println()
        })
    }
  }
}

object ConsumeStringSpec {
  def tokenize(in: String): List[String] = {
    var out = List[String]()
    val builder = new StringBuilder()
    for(c <- in) {
      if (c.isWhitespace) {
        if(builder.nonEmpty) out = out :+ builder.mkString
        out = out :+ c.toString
        builder.clear()
      } else {
        builder.append(c)
      }
    }

    if (builder.nonEmpty) {
      out = out :+ builder.mkString
    }
    out
  }

  def tokenize2(in: String): List[String] = in.foldLeft((List.empty[String], new StringBuilder)) {
    case ((out, builder), c) if c.isWhitespace =>
      val token = if (builder.nonEmpty) List(builder.mkString) else Nil
      (out ++ token :+ c.toString, new StringBuilder)
    case ((out, builder), c) =>
      builder.append(c)
      (out, builder)
  } match {
    case (out, builder) if builder.nonEmpty => out :+ builder.mkString
    case (out, _) => out
  }

  sealed trait Token
  case class WhiteSpace(c: String) extends Token
  case class Str(s: String) extends Token
  case class Punctuation(s: String) extends Token

  object Token {
    def isWhitespace(v: String): Boolean = v.length == 1 && v(0).isWhitespace
    def isPunctuation(v: String): Boolean = v.length == 1 && (!v(0).isLetterOrDigit && !v(0).isWhitespace)
  }

  def classify(s: String): Token = s match {
    case v if isWhitespace(v) => WhiteSpace(s)
    case v if isPunctuation(v) => Punctuation(s)
    case v  => Str(v)
  }

  def anonymise(s: List[String]): List[String] = s
    .map(classify)
    .map {
      case WhiteSpace(c) => c
      case Str(_) => "***"
      case Punctuation(s) => s
    }
}