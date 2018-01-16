package parsing

import fastparse.all._
import fastparse.core.Parsed
import org.scalatest.FunSuite

import scala.collection.mutable.ArrayBuffer

// For testing small parts of the CParser
class IndividualParserSpec extends FunSuite {
  val pp = createParser()

  /*

   Expression Statement------
               Identifier-
   Identifier  IntConstant
   return      0            ;
   */

  def dump[T, Elem, Repr](raw: String, p: Parsed[T, Elem, Repr]): Unit = {
    p match {
      case Parsed.Success(x, y) =>
        println(p)
      case Parsed.Failure(x, y, z) =>
        //        println(p)
        //        println(x)
        //        println(y)
        //        println(z)
        val traced = z.traced
        //        println(traced)

        for (i <- Range(0, traced.fullStack.size)) {
          val stack = traced.fullStack(i)
          //          val input = z.input.asInstanceOf[utils.IndexedParserInput].data.toString.substring(stack.index, stack.index + 20)
          val input = raw.substring(stack.index, Math.min(stack.index + 20, raw.length - 1)).replace("\r\n", """\r\n""")
          println(s"$i '$input' $stack")
        }

        println(s"All parsers tried at failure: ${traced.traceParsers.mkString(", ")}")

        assert (false)
    }
  }


  def good[T, Elem, Repr](p: Parsed[T, Elem, Repr], expected: T): Unit = {
    p match {
      case Parsed.Success(x, y) => assert (x == expected)
      case Parsed.Failure(x, y, z) =>
        //        println(s"Wanted:  ${expected}")
        //        dump("", p)
        val wrap = CParseResult.wrapFailed(p)
        println(wrap)
        val traced = z.traced
        println(s"Parse failure: ${z.traced}")
        //        println(p)
        //        println(x)
        //        println(y)
        //        println(z)
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

  //  test("hexadecimalDigit") {
  //    val p = createParser()
  //    good(p.hexadecimalDigit.parse("d"), HexDigit('d'))
  //    good(p.hexadecimalDigit.parse("0"), HexDigit('0'))
  //    bad(p.hexadecimalDigit.parse("g"))
  //  }
  //
  //  test("hexQuad") {
  //    val p = createParser()
  //    good(p.hexQuad.parse("d01d"), HexQuad(HexDigit('d'),HexDigit('0'),HexDigit('1'),HexDigit('d')))
  //    bad(p.hexQuad.parse("g"))
  //  }


  test("return 0") {
    val p = createParser()
    good(p.statement.parse("return 0;"), Return(Some(IntConstant(0))))
    good(p.blockItemList.parse("return 0;"), List(Return(Some(IntConstant(0)))))
  }


  test("0") {
    val p = createParser()
    good(p.octalConstant.parse("0"), IntConstant(0))
    good(p.integerConstant.parse("0"), IntConstant(0))
    good(p.constant.parse("0"), IntConstant(0))
    good(p.primaryExpression.parse("0"), IntConstant(0))
    good(p.expression.parse("0"), IntConstant(0))
  }

  //  test("nonDigit") {
  //    val p = createParser()
  //    good(p.nondigit.parse("A"), Nondigit('A'))
  //    good(p.nondigit.parse("a"), Nondigit('a'))
  //    bad(p.nondigit.parse("0"))
  //  }
  //
  //  test("identifierNondigit") {
  //    val p = createParser()
  //    good(p.identifierNondigit.parse("A"), IdentifierNondigit1(Nondigit('A')))
  //    good(p.identifierNondigit.parse("a"), IdentifierNondigit1(Nondigit('a')))
  //    bad(p.identifierNondigit.parse("0"))
  //  }

  test("identifier") {
    val p = createParser()
    good(p.identifier.parse("A"), Identifier("A"))
    good(p.identifier.parse("yabba"), Identifier("yabba"))
    good(p.identifier.parse("yabba3"), Identifier("yabba3"))
    good(p.identifier.parse("yabba3doo"), Identifier("yabba3doo"))
    bad(p.identifier.parse("3yabba"))
  }

  test("decimalFloatingConstant") {
    val p = createParser()
    assert(Math.abs(p.decimalFloatingConstant.parse("123.12312").get.value.v - 123.12312f) < 0.01)
    assert(Math.abs(p.decimalFloatingConstant.parse("123.12E1").get.value.v - 1231.2) < 0.01)
    assert(Math.abs(p.decimalFloatingConstant.parse("123E1").get.value.v - 1230f) < 0.01)
  }

  test("floatingConstant") {
    val p = createParser()
    assert(Math.abs(p.floatingConstant.parse("123.12312").get.value.v - 123.12312f) < 0.01)
    assert(Math.abs(p.floatingConstant.parse("123.12E1").get.value.v - 1231.2) < 0.01)
    assert(Math.abs(p.floatingConstant.parse("123E1").get.value.v - 1230f) < 0.01)
  }

  test("exponentPart") {
    val p = createParser()
    good(p.exponentPart.parse("e123"), Exponent(123))
    good(p.exponentPart.parse("E123"), Exponent(123))
    good(p.exponentPart.parse("E-123"), Exponent(-123))
  }

  test("stringLiteral") {
    val p = createParser()
    good(p.stringLiteral.parse(""""hello""""), StringLiteral("hello"))
    good(p.stringLiteral.parse("""u"hello""""), StringLiteral("hello"))
  }

  test("token") {
    val p = createParser()
    good(p.token.parse(""""hello""""), StringLiteral("hello"))
    good(p.token.parse("""inline"""), Keyword("inline"))
    good(p.token.parse("""|"""), Punctuator("|"))
  }

  test("headerName") {
    val p = createParser()
    good(p.headerName.parse(""""hello.h""""), HeaderName("hello.h"))
    good(p.headerName.parse("""<hello.h>"""), HeaderName("hello.h"))
    good(p.preprocessingToken.parse("""<hello.h>"""), HeaderName("hello.h"))
    good(p.ppTokens.parse("""<hello.h>"""), Seq(HeaderName("hello.h")))
  }

  test("jumpStatement") {
    val p = createParser()
    good(p.jumpStatement.parse("""continue;"""), Continue())
    //    good(p.jumpStatement.parse("""goto hello;"""), Goto(Identifier("hello")))
    good(p.jumpStatement.parse("""break;"""), Break())
    //    good(p.jumpStatement.parse("""return;"""), Break())
  }

  test("primaryExpression") {
    val p = createParser()
    good(p.primaryExpression.parse(""""hello""""), StringLiteral("hello"))
    good(p.primaryExpression.parse("""1234"""), IntConstant(1234))
    good(p.primaryExpression.parse("""1"""), IntConstant(1))
  }


  test("?") {
    val p = createParser()
    import fastparse.all._
    println((p.postfixExpression ~ End).parse("""hello++"""))
  }

  test("how to parse binary ops") {
    import fastparse.all._

    // Stackoverflow as we just loop through expr for every
    if (false) {
      val term: P[Any] = CharIn('0' to '9')
      val expr: P[Any] = P(expr ~ "+" ~ expr) | term
      val top = expr ~ End
      println(top.parse("1+1"))
    }

    // Only matches "1", doesn't match "+1"
    if (false) {
      val term: P[Any] = CharIn('0' to '9')
      val expr: P[Any] = term | P(expr ~ "+" ~ expr)
      val top = expr ~ End
      println(top.parse("1+1"))
    }

    // Works, but can only match +
    if (false) {
      val term: P[Any] = CharIn('0' to '9')
      val expr: P[Any] = term ~ P("+" ~ term).rep
      val top = expr ~ End
      println(top.parse("1+1"))
      println(top.parse("1+1+2"))
    }

    // Trying to get it matching + and -.  Fails, back to stackoverflow
    if (false) {
      val term: P[Any] = CharIn('0' to '9')
      lazy val add: P[Any] = expr ~ P("+" ~ expr).rep
      lazy val sub: P[Any] = expr ~ P("-" ~ expr).rep
      lazy val expr = add | sub
      val top = expr ~ End
      println(top.parse("1+1"))
      println(top.parse("1+1-2"))
    }

    // Attempting to refactor left recursion.  Working, not handling + and - yet.
    if (false) {
      val term: P[Any] = CharIn('0' to '9')
      lazy val add: P[Any] = term ~ add2
      lazy val add2: P[Any] = P(P("+") ~ add) | End
      val top = add ~ End
      println(top.parse("1+1"))
      println(top.parse("1+1+2"))
    }

    // Works!
    val term: P[Any] = CharIn('0' to '9')
    lazy val add: P[Any] = term ~ add2
    lazy val add2: P[Any] = P(P("+") ~ expr) | End
    lazy val sub: P[Any] = term ~ sub2
    lazy val sub2: P[Any] = P(P("-") ~ expr) | End
    lazy val expr = add | sub
    val top = expr ~ End
    println(top.parse("1+1"))
    println(top.parse("1+1+2"))
    println(top.parse("1+1-2"))

  }

  test("postfix++") {
    import fastparse.all._
    val parser = createParser()
    good((parser.postfixExpression ~ End).parse("hello++"), PostfixExpressionPlusPlus(Identifier("hello")))
  }

  test("postfixExpression ++ --") {
    import fastparse.all._
    val parser = createParser()
    good((parser.postfixExpression ~ End).parse("hello++"), PostfixExpressionPlusPlus(Identifier("hello")))
    good((parser.postfixExpression ~ End).parse("hello++--"), PostfixExpressionMinusMinus(PostfixExpressionPlusPlus(Identifier("hello"))))
    good((parser.postfixExpression ~ End).parse("hello++--++"), PostfixExpressionPlusPlus(PostfixExpressionMinusMinus(PostfixExpressionPlusPlus(Identifier("hello")))))
  }

  test("postfixExpression []") {
    import fastparse.all._
    val parser = createParser()
    good((parser.postfixExpression ~ End).parse("hello[1]"), PostfixExpressionIndex(Identifier("hello"), IntConstant(1)))
    good((parser.postfixExpression ~ End).parse("hello[world]"), PostfixExpressionIndex(Identifier("hello"), Identifier("world")))
    good((parser.postfixExpression ~ End).parse("hello[world]++"), PostfixExpressionPlusPlus(PostfixExpressionIndex(Identifier("hello"), Identifier("world"))))
  }

  test("postfixExpression .") {
    val parser = createParser()
    good((parser.postfixExpression ~ End).parse("hello.world"), PostfixExpressionDot(Identifier("hello"), Identifier("world")))
    good((parser.postfixExpression ~ End).parse("hello.world.again"), PostfixExpressionDot(PostfixExpressionDot(Identifier("hello"), Identifier("world")), Identifier("again")))
  }

  test("unaryExpression ++") {
    val parser = createParser()
    good((parser.unaryExpression ~ End).parse("++hello"), UnaryExpressionPlusPlus(Identifier("hello")))
    good((parser.unaryExpression ~ End).parse("--hello"), UnaryExpressionMinusMinus(Identifier("hello")))
    good((parser.unaryExpression ~ End).parse("++hello++"), UnaryExpressionPlusPlus(PostfixExpressionPlusPlus(Identifier("hello"))))
    //    good((CParser.postfixExpression ~ End).parse("hello.world.again"), PostfixExpressionDot(PostfixExpressionDot(Identifier("hello"), Identifier("world")), Identifier("again")))
  }

  test("typeName") {
    val parser = createParser()
    good((parser.typeName ~ End).parse("int"), TypeName("int"))
  }

  test("cast") {
    val parser = createParser()
    good((parser.castExpression ~ End).parse("(int)hello"), CastExpression(TypeName("int"), Identifier("hello")))
  }

  test("*") {
    val parser = createParser()
    good((parser.multiplicativeExpression ~ End).parse("hello*world"), ExpressionMultiply(Identifier("hello"), Identifier("world")))
    good((parser.multiplicativeExpression ~ End).parse("hello*world*3"), ExpressionMultiply(Identifier("hello"), ExpressionMultiply(Identifier("world"),IntConstant(3))))
    good((parser.multiplicativeExpression ~ End).parse("hello*world*3++"), ExpressionMultiply(Identifier("hello"), ExpressionMultiply(Identifier("world"),PostfixExpressionPlusPlus(IntConstant(3)))))
    good((parser.multiplicativeExpression ~ End).parse("hello*world*(3++)"), ExpressionMultiply(Identifier("hello"), ExpressionMultiply(Identifier("world"),PostfixExpressionPlusPlus(IntConstant(3)))))
  }

  test("=") {
    val parser = createParser()
    good((parser.assignmentExpression ~ End).parse("hello=world"), ExpressionAssignment(Identifier("hello"), Identifier("world")))
    good((parser.assignmentExpression ~ End).parse("hello*=world"), ExpressionAssignment(Identifier("hello"), ExpressionMultiply(Identifier("hello"), Identifier("world"))))
  }

  test("top level expression") {
    val parser = createParser()
    good((parser.expression ~ End).parse("hello=world"), ExpressionAssignment(Identifier("hello"), Identifier("world")))
    good((parser.expression ~ End).parse("hello*=world"), ExpressionAssignment(Identifier("hello"), ExpressionMultiply(Identifier("hello"), Identifier("world"))))
    good((parser.expression ~ End).parse("hello*world*(3++)"), ExpressionMultiply(Identifier("hello"), ExpressionMultiply(Identifier("world"),PostfixExpressionPlusPlus(IntConstant(3)))))
    good((parser.expression ~ End).parse("++hello++"), UnaryExpressionPlusPlus(PostfixExpressionPlusPlus(Identifier("hello"))))
  }

  test("top level statement") {
    val parser = createParser()
    good((parser.statement ~ End).parse("hello=world;"), ExpressionStatement(ExpressionAssignment(Identifier("hello"), Identifier("world"))))
    good((parser.statement ~ End).parse("hello*=world;"), ExpressionStatement(ExpressionAssignment(Identifier("hello"), ExpressionMultiply(Identifier("hello"), Identifier("world")))))
    good((parser.statement ~ End).parse("hello*world*(3++);"), ExpressionStatement(ExpressionMultiply(Identifier("hello"), ExpressionMultiply(Identifier("world"),PostfixExpressionPlusPlus(IntConstant(3))))))
    good((parser.statement ~ End).parse("++hello++;"), ExpressionStatement(UnaryExpressionPlusPlus(PostfixExpressionPlusPlus(Identifier("hello")))))
  }

  test("expressionStatement") {
    val parser = createParser()
    val x = ExpressionStatement(ExpressionAssignment(Identifier("hello"), Identifier("world")))
    good((parser.expressionStatement ~ End).parse("hello=world;"), x)
    good((parser.expressionStatement ~ End).parse(";"), ExpressionEmptyStatement())
  }

  test("if") {
    val parser = createParser()
    good((parser.selectionStatement ~ End).parse("if(hello==world)hello=1;"), SelectionIf(ExpressionEquals(Identifier("hello"), Identifier("world")), ExpressionStatement(ExpressionAssignment(Identifier("hello"), IntConstant(1)))))
  }

  test("whitespace") {
    val parser = createParser()
    good((parser.selectionStatement ~ End).parse("if(hello==world)hello=1;"), SelectionIf(ExpressionEquals(Identifier("hello"), Identifier("world")), ExpressionStatement(ExpressionAssignment(Identifier("hello"), IntConstant(1)))))
    good((parser.selectionStatement ~ End).parse("if(hello==world) hello=1;"), SelectionIf(ExpressionEquals(Identifier("hello"), Identifier("world")), ExpressionStatement(ExpressionAssignment(Identifier("hello"), IntConstant(1)))))
  }

  test("function nowhitespace") {
    val raw = """int main(int argc){return 0;}"""
    val p = createParser()
    //    dump((p.functionDefinition ~ End).parse(raw))


    //    val parsed = (p.functionDefinition ~ End).parseRec(new ParseCtx(utils.IndexedParserInput[Char,String](raw), 0, -1, p, 0, null, false, false, false), 0)
    //    1==1
    //    good(p.top.parse(raw), HexDigit('d'))
  }

  test("function whitespace") {
    val raw = """int main(int argc) { return 0; }"""
    val p = createParser()
    println((p.functionDefinition ~ End).parse(raw))
    //    good(p.top.parse(raw), HexDigit('d'))
  }

  //  test("function newlines") {
  //    val raw =
  //      """int main(int argc) {
  //        | return 0;
  //        |}""".stripMargin
  //    val p = createParser()
  //    //    good((p.declarationSpecifiers ~ End).parse("int"), DeclarationSpecifiers(Seq(TypeSpecifier("int"))))
  //    //    good((p.declarator ~ End).parse("main"), Declarator("main"))
  ////    println((p.functionDefinition ~ End).parse(raw))
  //        dump(raw, (p.functionDefinition ~ End).parse(raw, 0, (a,index,continuation) => {
  //          continuation() match {
  //            case Parsed.Success(x, y) =>
  //              println(s"$index to $y = $x")
  //            case _ =>
  //          }
  //        }))
  //  }

  test("parameterTypeList") {
    val p = createParser()
    good((p.parameterTypeList ~ End).parse("int argc"),
      ParameterTypeList(
        Seq(ParameterDeclarationDeclarator(
          DeclarationSpecifiers(Seq(TypeSpecifierSimple("int"))),
          Declarator(None, DirectDeclaratorOnly(Identifier("argc"))))),
        false
      )
    )
  }

  test("directDeclaratorHelper") {
    val p = createParser()
    val parsed = (p.directDeclaratorHelper ~ End).parse("(int argc)")
    good(parsed,
      p.DDBuild2(
        p.DDBuildParameterTypeList(
          ParameterTypeList(
            List(ParameterDeclarationDeclarator(
              DeclarationSpecifiers(List(TypeSpecifierSimple("int"))),
              Declarator(None, DirectDeclaratorOnly(Identifier("argc"))))),
            false
          )
        ),
        p.DDBuild2(p.Empty(),null)
      )
    )
  }

  test("declarator") {
    val p = createParser()
    //    good((p.directDeclaratorHelper ~ End).parse("(int argc)"), Declarator("main"))
    val raw = "main(int argc)"
    //    dump(raw, (p.declarator ~ End).parse(raw, 0, (a,index,continuation) => {
    //      continuation() match {
    //        case Parsed.Success(x, y) =>
    //          if (x.toString != "()") {
    //            println(s"MATCH $index to $y on $a $x")
    //          }
    //        case _ =>
    //      }
    //    })
    //    )

    good((p.declarator ~ End).parse(raw),
      Declarator(None,
        FunctionDeclaration(Identifier("main"),
          ParameterTypeList(
            List(ParameterDeclarationDeclarator(
              DeclarationSpecifiers(List(TypeSpecifierSimple("int"))),
              Declarator(None, DirectDeclaratorOnly(Identifier("argc"))))),
            false
          )
        )
      )
    )
  }

  test("int hello=3") {
    val p = createParser()
    good(p.blockItem.parse("int hello=3;"),
      SimpleDeclaration(
        DeclarationSpecifiers(List(TypeSpecifierSimple("int"))),
        Some(List(
          DeclaratorWithInit(
            Declarator(
              None,
              DirectDeclaratorOnly(Identifier("hello"))),
            InitializerSimple(IntConstant(3)
            )
          )
        ))
      )
    )
  }

  val helloDec = SimpleDeclaration(
    DeclarationSpecifiers(List(TypeSpecifierSimple("int"))),
    Some(List(DeclaratorEmpty(Declarator(None,DirectDeclaratorOnly(Identifier("hello")))))))

  test("int hello") {
    val p = createParser()
    good(p.blockItem.parse("int hello;"),helloDec)
  }


  test("comment end") {
    val p = createParser()
    good((p.declaration ~ End).parse("int hello;/*comment*/"), helloDec)
  }

  test("comment middle") {
    val p = createParser()
    good((p.declaration ~ End).parse("int/*comment*/hello;"), helloDec)
  }

  test("comment start") {
    val p = createParser()
    good((p.declaration ~ End).parse("/*comment*/int hello;"), helloDec)
  }

  test("comment single line") {
    val p = createParser()
    good((p.declaration).parse("int hello;//i'm a comment"), helloDec)
    good((p.declaration).parse("int hello; //i'm a comment"), helloDec)
    good((p.declaration).parse("int hello; // i'm a comment"), helloDec)
  }

  test("comment testing 1") {
    P(((!"*/" ~ AnyChar).rep ~/ "*/")).parse("/* */").get
    P(P("/*" ~/ (!"*/" ~ AnyChar).rep ~/ "*/")).parse("/* */").get
    P(P("/*" ~/ (!"*/" ~ AnyChar).rep ~/ "*/")).parse("/* hello */").get
    P(P("/*" ~/ (!"*/" ~ AnyChar).rep ~/ "*/")).parse("/* hello world */").get
  }

  test("include") {
    val p = createParser()
//    good(p.controlLine.parse("#include <hello.h>\n"), Include(Seq(HeaderName("hello.h"))))
//    good(p.preprocessingFile.parse("#include <hello.h>\n"), PreprocessingFile(Some(Group(Seq(Include(Seq(HeaderName("hello.h"))))))))
//    good(p.top.parse("#include <hello.h>\n"), PreprocessingFile(Some(Group(Seq(Include(Seq(HeaderName("hello.h"))))))))

    good(p.controlLine.parse("#include <hello.h>\n"), Include(Seq(HeaderName("hello.h"))))
//    good(p.controlLine.parse("#include<stdio.h>\n"), Include(Seq(HeaderName("stdio.h"))))
//    good(p.preprocessingFile.parse("#include <hello.h>\n"), PreprocessingFile(Some(Group(Seq(Include(Seq(HeaderName("hello.h"))))))))
//    good(p.top.parse("#include <hello.h>\n"), PreprocessingFile(Some(Group(Seq(Include(Seq(HeaderName("hello.h"))))))))
  }

  test("define") {
    val p = createParser()
//    good(p.controlLine.parse("#include <hello.h>\n"), Include(Seq(HeaderName("hello.h"))))
    good(p.controlLine.parse("#include <hello.h>\n"), Include(Seq(HeaderName("hello.h"))))
  }

  test("struct simple") {
    good(pp.typeSpecifier.parse("struct myStruct"), StructOrUnionSpecifier(true,Some(Identifier("myStruct")),List()))
    good(pp.structDeclaration.parse("int data;"), StructDeclaration(
      ArrayBuffer(
        TypeSpecifierSimple("int")),
      Some(StructDeclaratorList(
        List(
          StructDeclaractor1(
            Declarator(None,DirectDeclaratorOnly(Identifier("data")))))))))
  }

  def check[T](parser: Parser[T], raw: String) = {
    val parsed = parser.parse(raw)
    val wrap = CParseResult.wrap(parsed)
    wrap match {
      case CParseSuccess(x) =>
        assert (true)
      //        assert (x.v.nonEmpty)
      case CParseFail(x) =>
        println(wrap)
        assert (false)
    }

  }

  def check(raw: String) = {
    val p = createParser()
    val parsed = p.parse(raw)
    parsed match {
      case CParseSuccess(x) =>
        assert (true)
//        assert (x.v.nonEmpty)
      case CParseFail(x) =>
        println(parsed)
        assert (false)
    }
  }

  test("struct full easy") {
    val raw = """struct node { int data; }""".stripMargin

    good((pp.structOrUnionSpecifier ~ End).parse(raw), StructOrUnionSpecifier(true,Some(Identifier("node")),List(StructDeclaration(ArrayBuffer(TypeSpecifierSimple("int")),Some(StructDeclaratorList(List(StructDeclaractor1(Declarator(None,DirectDeclaratorOnly(Identifier("data")))))))))))
  }

  test("struct full complex") {
    val raw = """struct node { int data; struct node *next; }""".stripMargin
    good((pp.structOrUnionSpecifier ~ End).parse(raw),
      StructOrUnionSpecifier(true,
        Some(Identifier("node")),
        List(
          StructDeclaration(List(
            TypeSpecifierSimple("int")),
            Some(StructDeclaratorList(
              List(
                StructDeclaractor1(
                  Declarator(None,DirectDeclaratorOnly(Identifier("data")))))))),
            StructDeclaration(
            List(
              TypeSpecifierSimple("struct node")),
            Some(StructDeclaratorList(List(
              StructDeclaractor1(Declarator(Some("*"),DirectDeclaratorOnly(Identifier("next")))))))))))
  }

  test("struct with var") {
    val raw = """struct node { int data; } *head;""".stripMargin

    good((pp.declaration ~ End).parse(raw), SimpleDeclaration(DeclarationSpecifiers(List(StructOrUnionSpecifier(true,Some(Identifier("node")),List(StructDeclaration(List(TypeSpecifierSimple("int")),Some(StructDeclaratorList(List(StructDeclaractor1(Declarator(None,DirectDeclaratorOnly(Identifier("data")))))))))))),Some(List(DeclaratorEmpty(Declarator(Some("*"),DirectDeclaratorOnly(Identifier("head"))))))))
  }

  test("while") {
    val raw = """while(n!=NULL) { n=n->next; c++; }""".stripMargin
    check(pp.statement ~ End, """{
                                |    n=n->next;
                                |    c++;
                                |    }""".stripMargin)
    check(pp.iterationStatement ~ End, raw)
  }

  test("just func stuff") {
    check(pp.blockItemList, """struct node *n;int c=0;n=head;""")
    val raw = """struct node *n;
                |    int c=0;
                |    n=head;""".stripMargin
    check(pp.blockItemList, raw)
  }

}
