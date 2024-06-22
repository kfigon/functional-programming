import org.scalatest.funspec.AnyFunSpec

class LexerSpec extends AnyFunSpec {

    List(
        (
          "basic",
          " 3helló () 31 foo fn fn3)",
          List(
              Identifier("3helló"),
              OpenPar,
              ClosePar,
              Number(31),
              Identifier("foo"),
              Keyword(Fn),
              Identifier("fn3"),
              ClosePar)
        ),
        (
          "trailing whitespaces",
          " fn fn3 ",
          List(Keyword(Fn), Identifier("fn3"))
        ),
        (
          "trailing num after keyword",
          " fn fn3",
          List(Keyword(Fn), Identifier("fn3"))
        ),
        (
          "parens at the end",
          "((xa)(",
          List(
              OpenPar,
              OpenPar,
              Identifier("xa"),
              ClosePar,
              OpenPar)
        ),
        (
          "operators",
          "= == != +3)",
          List(
              Operator(Assign),
              Operator(Eq),
              Operator(NotEq),
              Operator(Plus),
              Number(3),
              ClosePar)
        ),
        (
          "comparison",
          "foo(123) == someVal",
          List(
            Identifier("foo"),
            OpenPar,
            Number(123),
            ClosePar,
            Operator(Eq),
            Identifier("someVal")
          )
        )
    ).foreach { v =>
        it(v._1) {
            assert(Lexer.lex(v._2) == v._3)
        }
    }
}