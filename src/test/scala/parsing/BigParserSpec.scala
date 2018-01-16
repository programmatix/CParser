package CParser

import fastparse.core.Parsed
import org.scalatest.FunSuite
import parsing.{CParseFail, CParseSuccess, CParser}

// For testing bigger functions and whole files
class BigParserSpec extends FunSuite {

  def good[T, Elem, Repr](p: Parsed[T, Elem, Repr], expected: T): Unit = {
    p match {
      case Parsed.Success(x, y) => assert (x == expected)
      case Parsed.Failure(x, y, z) =>
        println(p)
        println(x)
        println(y)
        println(z)
        assert (false)
    }
  }

  def bad[T, Elem, Repr](p: Parsed[T, Elem, Repr]): Unit = {
    p match {
      case Parsed.Success(x, y) => assert (false)
      case Parsed.Failure(x, y, z) =>
    }
  }

  def createParser() = new CParser

  test("function") {
    val raw =
      """int main(int argc) {
        | return 0;
        |}
      """.stripMargin
      val p = createParser()
    p.functionDefinition.parse(raw).get
  }

  test("comment") {
    val raw = """// Replace all this with any valid C code
                |
                |int main(int argc) {
                |    return 0;
                |}
                |                """.stripMargin
    val p = createParser()
    p.parse(raw) match {
      case CParseSuccess(x) => assert (true)
      case CParseFail(x) => assert (false)
    }
  }
}