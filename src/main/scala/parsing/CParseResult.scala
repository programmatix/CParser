package parsing

import fastparse.core.Parsed
import fastparse.utils.IndexedParserInput

/**
  * Wraps fastparse's results, providing some extra debug info
  */
sealed trait CParseResult

case class CParseSuccess[T](t: T) extends CParseResult

case class CParseFail[T, Elem, Repr](parsed: Parsed[T, Elem, Repr]) extends CParseResult {
  val (err: String, failIndex: Int) = parsed match {
    case Parsed.Failure(x, failIndex: Int, z) =>
      val traced = z.traced
      val input = traced.input.asInstanceOf[IndexedParserInput[Char, String]].data
      val last = traced.fullStack.last
      val err = s"At index $failIndex '${input.substring(failIndex, Math.min(input.length, failIndex + 10))}' did not find expected '${last.parser.toString}'"
      (err, failIndex)
  }

  override def toString: String = err
}

object CParseResult {
  def wrap[T, Elem, Repr](in: Parsed[T, Elem, Repr]): CParseResult = {
    in match {
      case Parsed.Success(x, y)                 => CParseSuccess(x)
      case Parsed.Failure(x, failIndex: Int, z) => CParseFail(in)
    }
  }

  private[parsing] def wrapFailed[T, Elem, Repr](in: Parsed[T, Elem, Repr]): CParseFail[T,Elem,Repr] = {
    in match {
      case Parsed.Failure(x, failIndex: Int, z) => CParseFail(in)
      case _ =>
        assert(false)
        null
    }
  }
}