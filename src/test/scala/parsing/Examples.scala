package parsing

import org.scalatest.FunSuite

class Examples extends FunSuite {
  test("parse") {
    val parser = new CParser()
    val parsed = parser.parse(
      """int main(int argc) {
        | return 0;
        |}""".stripMargin)

    // `parsed` can be CParseSuccess or CParseFail
    parsed match {
      case CParseSuccess(result: CFile) =>

        // If success, the result is an abstract syntax tree corresponding to the C code
        assert (result ==
          TranslationUnit(
            List(
              FunctionDefinition(
                DeclarationSpecifiers(
                  List(
                    TypeSpecifier("int"))),
                Declarator(None,
                  FunctionDeclaration(
                    Identifier("main"),
                    ParameterTypeList(
                      List(
                        ParameterDeclarationDeclarator(
                          DeclarationSpecifiers(
                            List(
                              TypeSpecifier("int"))),
                          Declarator(
                            None,
                            DirectDeclaratorOnly(
                              Identifier("argc"))))),
                      ellipses = false))),
                None,
                CompoundStatement(
                  List(
                    Return(
                      Some(IntConstant(0))))))))
        )

      case CParseFail(result) =>
        println(result)
    }
  }

  test("parseSnippet") {
    val parser = new CParser()
    val parsed = parser.parseSnippet("int hello;")

    // `parsed` can be CParseSuccess or CParseFail
    parsed match {
      case CParseSuccess(result: Seq[BlockItem]) =>

        // If success, the result is an abstract syntax tree corresponding to the C code
        assert (result.head ==
          SimpleDeclaration(
            DeclarationSpecifiers(
              List(
                TypeSpecifier("int"))),
            Some(
              List(
                DeclaratorEmpty(
                  Declarator(
                    None,
                    DirectDeclaratorOnly(
                      Identifier("hello")))))))
        )

      case CParseFail(result) =>
        println(result)
    }
  }

}