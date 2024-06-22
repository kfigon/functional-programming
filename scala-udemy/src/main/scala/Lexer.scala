import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

sealed trait Token
case object OpenPar extends Token
case object ClosePar extends Token
case class Identifier(v: String) extends Token
case class Keyword(v: KeywordEnum) extends Token
case class Number(v: Int) extends Token
case class Operator(v: OperatorEnum) extends Token

sealed trait KeywordEnum
case object Fn extends KeywordEnum
case object For extends KeywordEnum

sealed trait OperatorEnum
case object Plus extends OperatorEnum
case object Minus extends OperatorEnum
case object Assign extends OperatorEnum
case object Eq extends OperatorEnum
case object NotEq extends OperatorEnum
case object Bang extends OperatorEnum

object OperatorEnum {
  val opChars: Set[Char] = Set('+','-','=','!')
  // !(c.isLetterOrDigit || c.isWhitespace || c == '(' || c == ')')
  def validOperatorChar(c: Char): Boolean = opChars.contains(c)

  def get(v: String): Option[OperatorEnum] = v match {
    case "+" => Some(Plus)
    case "-" => Some(Minus)
    case "=" => Some(Assign)
    case "==" => Some(Eq)
    case "!=" => Some(NotEq)
    case "!" => Some(Bang)
    case _ => None
  }
}

object KeywordEnum {
  def get(v: String): Option[KeywordEnum] = v match {
    case "for" => Some(For)
    case "fn" => Some(Fn)
    case _ => None
  }
}

object Lexer {
  def lex(in: String): List[Token] = {
    val out = ArrayBuffer[Token]()
    val it = in.iterator.buffered

    while (it.hasNext) {
      toTok(it) match {
        case Some(value) => out += value
        case None => ()
      }
    }
    out.toList
  }

  private def toTok(it: BufferedIterator[Char]): Option[Token] = it.next match {
    case c if c.isWhitespace => None
    case ')' => Some(ClosePar)
    case '(' => Some(OpenPar)
    case c if OperatorEnum.opChars.contains(c) =>
      val ops = c + consumeWhile(it, OperatorEnum.validOperatorChar)
      OperatorEnum.get(ops).map(Operator)
    case other =>
      val word = other + consumeWhile(it, _.isLetterOrDigit)
      KeywordEnum.get(word) match {
        case Some(value) => Some(Keyword(value))
        case None =>
          Try(word.toInt) match {
            case Success(value) => Some(Number(value))
            case Failure(_) => Some(Identifier(word))
          }
      }
  }

  private def consumeWhile(it: BufferedIterator[Char], fn: Char => Boolean): String = {
    // this consumes the next char, so fn3) will skip the parenthesis!
    // it.takeWhile(_.isLetterOrDigit).mkString

    val wordBuffer = new StringBuilder()
    // returns next without advancing
    while (it.headOption.exists(fn)) {
      wordBuffer += it.next()
    }
    wordBuffer.mkString
  }
}