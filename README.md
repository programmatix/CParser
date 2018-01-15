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
val parsed = p.blockItem.parse("int hello;")

// `parsed` comes directly from the fastparse library
parsed match {
    case Parsed.Success(result, index) =>
        assert (result ==
              SimpleDeclaration(
                DeclarationSpecifiers(List(TypeSpecifier("int"))),
                Some(List(
                    DeclaratorEmpty(
                        Declarator(
                            None,
                            DirectDeclaratorOnly(Identifier("hello"))
                        )
                    )
                ))
              )
            )
        )

    case Parsed.Failure(result, index, failure) =>
        println(failure)
    }
}
```

## C Language
This parser understands the C language grammar defined in [Annex A of this C specification](https://port70.net/~nsz/c/c11/n1570.html#A).

The grammar needed to be refactored somewhat to be parsable.  The biggest change was removing left-recursion throughout.  Overall though, you won't have much problem matching up the code with the spec.

## Parsers
CParser contains a lot of different parsers, corresponding to each part of the spec. 

The most useful parsers are:

* parser.translationUnit - this understands a complete C file
* parser.blockItem - understands a single line of C inside a function ("int myVar = 0;")
* parser.blockItemList - understands a Seq of multiple blockItems

## Using The Results
Each parser returns a result from the fastparse library, so take a look at the [fastparse docs](http://www.lihaoyi.com/fastparse/).

Wrapped inside a successful fastparse result is an abstract syntax tree (AST) corresponding to the C specification.  

The top-level of the C grammar is translation-unit:

```
(6.9) translation-unit:
                external-declaration
                translation-unit external-declaration
```   

So the parser.translationUnit parser will return a TranslationUnit() case class, containing a list of ExternalDeclaration():

```
case class TranslationUnit(v: Seq[ExternalDeclaration])
```

You can walk through these structures, referring to the code and spec to see the options at each stage.  It's not worth documenting every class as they're simple wrappers corresponding nearly 1-to-1 with the C grammar.  

## Contributing

Please feel free to send PRs!

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details
