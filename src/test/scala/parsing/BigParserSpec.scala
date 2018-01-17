package parsing

import fastparse.core.Parsed
import org.scalatest.FunSuite

// For testing bigger functions and whole files
class BigParserSpec extends FunSuite {

  def good[T, Elem, Repr](p: Parsed[T, Elem, Repr], expected: T): Unit = TestUtils.good(p, expected)

  def bad[T, Elem, Repr](p: Parsed[T, Elem, Repr]): Unit = TestUtils.bad(p)

  def createParser() = TestUtils.createParser()

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

  test("struct") {
    val raw = """
                |struct node
                |{
                |    int data;
                |    struct node *next;
                |}*head;""".stripMargin
    val p = createParser()
    val parsed = p.parse(raw)
    parsed match {
      case CParseSuccess(x) => assert (true)
      case CParseFail(x) =>
        println(parsed)
        assert (false)
    }
  }

  def check(raw: String, print: Boolean = false): Unit = TestUtils.check(raw, print)

  test("single linked list") {
    TestUtils.checkFile(this.getClass.getResource("/Snippet1.c"), true)
  }

  test("single linked list insert") {
    TestUtils.checkFile(this.getClass.getResource("/Snippet2.c"), true)
  }

  test("count while") {
    check("""int count()
            |{
            |    struct node *n;
            |    int c=0;
            |    n=head;
            |    while(n!=NULL)
            |    {
            |    n=n->next;
            |    c++;
            |    }
            |    return c;
            |}""".stripMargin)
  }

  test("simple") {
    check("int count() { return 0; }")
    check(
      """int count()
        |{
        |    return 0;
        |}""".stripMargin)
  }

  test("include and struct simple") {
    val raw = """#include<stdio.h>
                |
                |struct node
                |{
                |    int data;
                |};
                |""".stripMargin
    val out = TestUtils.check(raw, print = true)
    assert (TestUtils.contains[Group](out))
    assert (TestUtils.contains[StructOrUnionSpecifier](out))
  }

  test("include and struct") {
    val raw = """#include<stdio.h>
                |#include<stdlib.h>
                |
                |struct node
                |{
                |    int data;
                |    struct node *next;
                |}*head;
                |""".stripMargin
    val out = TestUtils.check(raw)
    assert (TestUtils.contains[Group](out))
    assert (TestUtils.contains[StructOrUnionSpecifier](out))
  }
}