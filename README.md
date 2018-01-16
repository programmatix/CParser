# CParser

Parses the C language into a clean abstract syntax tree that you can use in your JVM project. 

Written in Scala, and ScalaJS compatible.

## Getting Started

Clone the project.  Add the single required dependency on the lihaoyi's excellent [fastparse library](https://github.com/lihaoyi/fastparse) to your build.sbt:

```
libraryDependencies += "com.lihaoyi" %%% "fastparse" % "1.0.0"
```

There's two main entry points.  CParser.parseSnippet is for parsing a snippet of C code - something you'd find inside a C function.
CParser.parse is for parsing a C translation unite - which can be as small as a single function, or as big as a full C file. 

Both functions return a CParseSuccess if they can parse the input, containing an abstract syntax tree in the form of nested case classes.
parseSnippet returns a Seq of BlockItem, each corresponding to block-item in the C specification, which roughly means "a line of C code". 

```
    val parser = new CParser()
    val parsed: CParseResult = parser.parseSnippet("int hello;")

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
```

parse returns a TranslationUnit, corresponding to translation-unit in the spec, which roughly means 'a complete C file'.

```
    val parser = new CParser()
    val parsed: CParseResult = parser.parse(
      """int main(int argc) {
        | return 0;
        |}""".stripMargin)

    // `parsed` can be CParseSuccess or CParseFail
    parsed match {
      case CParseSuccess(result: TranslationUnit) =>

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
```

## C Language
This parser understands the C language grammar defined in [Annex A of this C specification](https://port70.net/~nsz/c/c11/n1570.html#A).

The grammar needed to be refactored somewhat to be parsable.  The biggest change was removing left-recursion throughout.  Overall though, you won't have much problem matching up the code with the spec.

## Granular Parsers
CParser contains a lot of different parsers, corresponding to each part of the spec. 

It's best to use either parser.parse (for a C file) or parser.parseSnippet (for the contents of a C function).  These return CParseResults.

But you're free to use the individual parsers if needed.  These will return fastparse results, so take a look at the [fastparse docs](http://www.lihaoyi.com/fastparse/) for more on how to handle them.
 
The most useful parsers are:

* parser.translationUnit - this understands a complete C file
* parser.blockItem - understands a single line of C inside a function ("int myVar = 0;")
* parser.blockItemList - understands a Seq of multiple blockItems

## Using The Results
parse and parseSnippet return a CParseResult, which can be either a CParseSuccess or a CParseFail. 

A CParseSuccess contains an abstract syntax tree (AST) corresponding to the C specification.  These are best explained with an example:  

The top-level of the C grammar is translation-unit:

```
(6.9) translation-unit:
                external-declaration
                translation-unit external-declaration
```   

This says that a translation-unit is made up of a list of at least one external-declaration's.

So the parser.translationUnit parser will return a TranslationUnit() case class, containing a list of ExternalDeclaration():

```
case class TranslationUnit(v: Seq[ExternalDeclaration])
```

You can walk through these structures, referring to the code and spec to see the options at each stage.  Every class is a simple wrapper corresponding nearly 1-to-1 with the C grammar.  

A CParseFail contains the index in the input string where parsing failed, along with an attempt to explain the failure.  As long as you're providing valid C code there shouldn't be parse errors - please raise an issue if you do see them.  

## Contributing

Please feel free to send PRs!

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
