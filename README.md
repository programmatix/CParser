# CParser

Parses the C language into a clean abstract syntax tree that you can use in your JVM project. 

Written in Scala, and ScalaJS compatible.

## Getting Started

Clone the project.  Add the single required dependency on the lihaoyi's excellent [fastparse library](https://github.com/lihaoyi/fastparse) to your build.sbt:

```
libraryDependencies += "com.lihaoyi" %%% "fastparse" % "1.0.0"
```

And then start playing:

```
val parser = new CParser()
val parsed: CParseResult = parser.parseSnippet("int hello;")

// `parsed` can be CParseSuccess or CParseFail
parsed match {
  case CParseSuccess(result) =>

    // If success, the result is an abstract syntax tree corresponding to the C code
    assert (result ==
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

## C Language
This parser understands the C language grammar defined in [Annex A of this C specification](https://port70.net/~nsz/c/c11/n1570.html#A).

The grammar needed to be refactored somewhat to be parsable.  The biggest change was removing left-recursion throughout.  Overall though, you won't have much problem matching up the code with the spec.

## Parsers
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
