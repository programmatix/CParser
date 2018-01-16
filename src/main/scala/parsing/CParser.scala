package parsing

/** This parses the C language according to the grammar here
  * https://port70.net/~nsz/c/c11/n1570.html#A
  * Converting an input string into an abstract syntax tree.
  * The grammar has been tweaked slightly, namely left recursion has been removed.
  *
  * You may want to use any of the parsers so they're all public.  If you're looking for a simple top-level starting point,
  * use `parse` or `parseSnippet`.
  */
class CParser {

  // These are used temporarily while creating the AST but don't form part of the final AST
  private[parsing] case class PostfixLeft(v: Expression)
  private[parsing] sealed trait PostfixRight
  private[parsing] case class PostfixRightIndex(v1: Expression) extends PostfixRight
  private[parsing] case class PostfixRightDot(v1: Expression) extends PostfixRight
  private[parsing] case class PostfixRightPlusPlus() extends PostfixRight
  private[parsing] case class PostfixRightMinusMinus() extends PostfixRight
  private[parsing] case class PostfixRightArrow(v1: Expression) extends PostfixRight
  private[parsing] case class PostfixRightArgs(v2: Option[ArgumentExpressionList]) extends PostfixRight
  private[parsing] case class PostfixRight2(op: PostfixRight, next: PostfixRight2)
  private[parsing] sealed trait DDBuild
  private[parsing] case class DDBuild2(me: DDBuild, next: DDBuild2)
  private[parsing] case class DDBuildParameterTypeList(v: ParameterTypeList) extends DDBuild
  private[parsing] case class DDBuildIdentifierList(v: Option[Seq[Identifier]]) extends DDBuild
  private[parsing] case class DDBuildTypeQualifierList(v: Option[Seq[TypeQualifier]]) extends DDBuild
  private[parsing] case class DDBuildTypeQualifierListAssignment(v: Option[Seq[TypeQualifier]], v2: Option[Expression]) extends DDBuild
  private[parsing] sealed trait MultiplicativeBuild
  private[parsing] case class BinaryOpBuildWrap(op: String, next: Expression)
  private[parsing] case class BinaryOpBuildWrap2(op: String, next: BinaryOpBuildWrap2)
  private[parsing] case class TernaryOpBuildWrap(op1: String, op2: String, v1: Expression, v2: Expression)
  private[parsing] case class Empty() extends PostfixRight with MultiplicativeBuild with DDBuild


  // Whitespace sensitive parsers go here
  val identifier: fastparse.all.Parser[Identifier] = {
    import fastparse.all._
    val digit = P(CharIn('0' to '9')).!.map(v => Digit(v.charAt(0)))
    val hexadecimalDigit = P(CharIn('0' to '9') | CharIn('a' to 'f') | CharIn('A' to 'F')).!.map(v => HexDigit(v.charAt(0)))
    val hexQuad = P(hexadecimalDigit ~ hexadecimalDigit ~ hexadecimalDigit ~ hexadecimalDigit).map(v => HexQuad(v._1, v._2, v._3, v._4))
    val universalCharacterName = P(P("\\u") ~ hexQuad).map(v => UniversalCharacterName1(v)) | P("\\U" ~ hexQuad ~ hexQuad).map(v => UniversalCharacterName2(v._1, v._2))
    val nondigit = P(CharIn('a' to 'z') | CharIn('A' to 'Z') | "_").!.map(v => Nondigit(v.charAt(0)))
    val identifierNondigit = nondigit.map(v => IdentifierNondigit1(v)) |
      universalCharacterName.map(v => IdentifierNondigit2(v))
    P(Index ~ P(nondigit ~ !" " ~ (nondigit | universalCharacterName | digit).rep(0)).! ~ Index).opaque("identifier").map(v =>
      Identifier(v._2))
  }

  val decimalConstant = {
    import fastparse.all._
    P(CharIn('1' to '9') ~ CharIn('0' to '9').rep(0)).!.map(v =>
      IntConstant(Integer.parseInt(v, 10)))
  }
  val octalConstant = {
    import fastparse.all._
    P("0" ~ CharIn('0' to '7').rep(0)).!.map(v =>
      IntConstant(Integer.parseInt(v, 8)))
  }

  // Ignore whitespace
  private val White = fastparse.WhitespaceApi.Wrapper {
    import fastparse.all._
    val comment = P("/*" ~/ (!"*/" ~ AnyChar).rep ~/ "*/").opaque("comment")
    NoTrace(CharIn(" \t\n").rep | P("\r\n").rep | comment.rep)
  }
  import fastparse.noApi._
  import White._


  // Whitespace in-sensitive parsers go here

  // http://c0x.coding-guidelines.com/6.4.2.1.html
  lazy val keyword = P(P("auto ∗") | P("break") | P("case") | P("char") | P("const") | P("continue") | P("default") | P("do") | P("double") | P("else") |
    P("enum") | P("extern") | P("float") | P("for") | P("goto") | P("if") | P("inline") | P("int") | P("long") | P("register") | P("restrict") |
    P("return") | P("short") | P("signed") | P("sizeof") | P("static") | P("struct") | P("switch") | P("typedef") | P("union") | P("unsigned") |
    P("void") | P("volatile") | P("while") | P("_Alignas") | P("_Alignof") | P("_Atomic") | P("_Bool") | P("_Complex") | P("_Generic") |
    P("_Imaginary") | P("_Noreturn") | P("_Static_assert") | P("_Thread_local")).!.map(v => Keyword(v))


  lazy val constant: Parser[Constant] = P(integerConstant | floatingConstant).opaque("constant")
  lazy val integerConstant: Parser[IntConstant] =
    P(P(decimalConstant ~ integerSuffix.? | octalConstant ~ integerSuffix.? | hexadecimalConstant ~ integerSuffix.?))
      .opaque("integerConstant")
  lazy val hexadecimalPrefix = P("0x") | P("0X")
  lazy val hexadecimalConstant =
    P(hexadecimalPrefix ~ CharIn("0123456789abcdef").rep(1)).!.map(v =>
      IntConstant(Integer.parseInt(v, 16)))
  lazy val nonzeroDigit = P(CharIn('1' to '9')).!.map(v => Digit(v.charAt(0)))
  //   lazy val octalDigit = P(CharIn('0' to '7'))
  lazy val integerSuffix = P((CharIn("uU") ~ P(P("ll") | P("LL") | P("l") | P("L"))) |
    ((P("ll") | P("LL") | P("l") | P("L")) ~ P(CharIn("uU")).?))
  //   lazy val unsignedSuffix = P(CharIn("uU"))
  //   lazy val longSuffix = P(CharIn("lL"))
  //   lazy val longLongSuffix = P("ll" | "LL")
  lazy val floatingConstant: Parser[FloatConstant] = P(decimalFloatingConstant | hexadecimalFloatingConstant)
  lazy val decimalFloatingConstant: Parser[FloatConstant] = P(P(fractionalConstant ~ exponentPart.? ~ floatingSuffix.?).map(v => FloatConstant(v._1.v * v._2.map(_.calc()).getOrElse(1.0f))) |
    P(digitSequence.!.map(v => {
      v.toInt
    }) ~ exponentPart ~ floatingSuffix.?).map(v => FloatConstant(v._1 * v._2.calc())))
  lazy val hexadecimalFloatingConstant: Parser[FloatConstant] =
    P(hexadecimalPrefix ~ hexadecimalFractionalConstant ~ binaryExponentPart ~ floatingSuffix.?).map(v =>
      FloatConstant(v._1.v * v._2.calc())) |
      P(hexadecimalPrefix ~ hexadecimalDigitSequence ~ binaryExponentPart ~ floatingSuffix.?).map(v =>
        FloatConstant(v._1.v * v._2.calc()))
  lazy val fractionalConstant = P(digitSequence.? ~ P(".") ~ digitSequence).!.map(v => FloatConstant(v.toFloat))
  lazy val exponentPart: Parser[Exponent] =
    (P("e" | P("E")) ~ CharIn("+-").? ~ digitSequence).!.map(v => {
      Exponent(v.stripPrefix("e").stripPrefix("E").toInt)
    })
  lazy val digitSequence = P(CharIn("0123456789").rep(1))
  lazy val hexadecimalFractionalConstant: Parser[FloatConstant] =
    P(hexadecimalDigitSequence.?.! ~ P(".").! ~ hexadecimalDigitSequence.!).map(v =>
      FloatConstant(v._1.toFloat + Math.pow(v._3.toFloat, v._3.size * 1).toFloat))
  lazy val binaryExponentPart: Parser[Exponent] =
    (P("p" | P("P")) ~ CharIn("+-").? ~ digitSequence).!.map(v =>
      Exponent(v.stripPrefix("p").stripPrefix("P").toInt))
  lazy val hexadecimalDigitSequence: Parser[FloatConstant] =
    P(CharIn("0123456789abcdef").rep(1)).!.map(v => FloatConstant(v.toFloat))
  lazy val floatingSuffix = P(CharIn("flFL"))


  lazy val enumerationConstant = identifier.map(v => EnumerationConstant(v.v))
  lazy val cCharSequence = CharsWhile(v => v != '\'' && v != '\\' && v != '\n')
  lazy val characterConstant: Parser[CharacterConstant] =
    P(CharIn("LuU").? ~ P("\'") ~ cCharSequence ~ "\'").!.map(v =>
      CharacterConstant(v.stripPrefix("\"").stripSuffix("\"")))

  // TODO escape sequences
  //  lazy val escapeSequence = simpleEscapeSequence
  //  octalEscapeSequence
  //  hexadecimalEscapeSequence
  //  universalCharacterName
  //  lazy val simpleEscapeSequence: one of
  //    \' \" \? \\
  //  \a \b \f \n \r \t \v
  //  lazy val octalEscapeSequence = \ octalDigit
  //    \ octalDigit octalDigit
  //  \ octalDigit octalDigit octalDigit
  //  lazy val hexadecimalEscapeSequence = \x hexadecimalDigit
  //  hexadecimalEscapeSequence hexadecimalDigit
  //    A.1.6 String literals

  lazy val stringLiteral: Parser[StringLiteral] = P(encodingPrefix.? ~ CharIn("\"") ~ CharsWhile(v => v != '"' && v != '\\' && v != '\n') ~ CharIn("\"")).!.map(v =>
    StringLiteral(v.stripPrefix("u8").stripPrefix("u").stripPrefix("U").stripPrefix("L").stripPrefix("\"").stripSuffix("\"")))
  lazy val encodingPrefix = P("u8" | CharIn("uUL"))
  lazy val punctuator: Parser[Punctuator] =
    P(P("[") | P("|") | P("]") | P("(") | P(")") | P("{") | P("}") | P(".") | P("->") | P("++") | P("--") | P("&") |
      P("*") | P("+") | P("-") | P("~") | P("!") | P("/") | P("%") | P("<<") | P(">>") | P("<") | P(">") | P("<=") |
      P(">=") | P("==") | P("!=") | P("^") | P("|") | P("&&") | P("||") | P("?") | P(":") | P(";") | P("...") | P("=") |
      P("*=") | P("/=") | P("%=") | P("+=") | P("-=") | P("<<=") | P(">>=") | P("&=") | P("^=") | P("|=") | P(",") |
      P("#") | P("##") | P("<:") | P(":>") | P("<%") | P("%>") | P("%:") | P("%:%:")).!.map(v => Punctuator(v))

  lazy val token: Parser[Token] = keyword | identifier | constant | stringLiteral | punctuator

  lazy val headerName: Parser[HeaderName] = (P(P("<") ~ CharsWhile(v => v != '\n' && v != '>') ~ P(">")) |
    P(P("\"") ~ CharsWhile(v => v != '\n' && v != '"') ~ P("\""))).!.map(v => {
    HeaderName(v.substring(1, v.size - 1))
  })

  // TODO preprocessor
  //  lazy val ppNumber = digit
  //    . digit
  //  ppNumber digit
  //    ppNumber identifierNondigit
  //    ppNumber e sign
  //  ppNumber E sign
  //  ppNumber p sign
  //  ppNumber P sign
  //  ppNumber .

  //  lazy val primaryExpression: Parser[Expression] = P(identifier | constant | stringLiteral | P(P("(") ~ expression ~ P(")")) | genericSelection)
  lazy val primaryExpression: Parser[Expression] =
    P(identifier | constant | stringLiteral | P(P("(") ~ expression ~ P(")"))).opaque("primaryExpression")

  // Never seen this, ignoring
  //  lazy val genericSelection: Parser[GenericSelection] = P("_Generic") ~ P("(") ~ assignmentExpression ~ P(",") ~ genericAssocList ~ P(")")
  //  lazy val genericAssocList: Parser[Any] = genericAssociation | P(genericAssocList ~ P(",") ~ genericAssociation)
  //  lazy val genericAssociation = P(typeName ~ P(":") ~ assignmentExpression) |
  //    P(P("default") ~ P(":") ~ assignmentExpression)

  lazy val postfixExpression: Parser[Expression] =
    P(primaryExpression ~ postfixExpressionR).map(v =>
      postfixRecurse(v)).opaque("postfixExpression")

  private def postfixRecurse(v: (Expression, PostfixRight2)): Expression = {
    val exp = postfixMerge(v._1, v._2.op)
    if (v._2.next == null) {
      v._1
    }
    else {
      v._2.next.op match {
        case _: Empty => exp
        case _        => postfixRecurse(exp, v._2.next)
      }
    }
  }

  def postfixMerge(left: Expression, v: PostfixRight): Expression = {
    val exp: Expression = v match {
      case x: PostfixRightIndex      => PostfixExpressionIndex(left, x.v1)
      case x: PostfixRightDot        => PostfixExpressionDot(left, x.v1)
      case x: PostfixRightPlusPlus   => PostfixExpressionPlusPlus(left)
      case x: PostfixRightMinusMinus => PostfixExpressionMinusMinus(left)
      case x: PostfixRightArrow      => PostfixExpressionArrow(left, x.v1)
      case x: PostfixRightArgs       => PostfixExpressionArgs(left, x.v2)
      case x: Empty                  => PostfixExpressionSimple(left)
    }
    exp
  }

  lazy val postfixExpressionR: Parser[PostfixRight2] =
    P(P("[") ~ expression ~ P("]") ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightIndex(v._1), v._2)) |
      //    P(P("[") ~ multiplicativeExpression ~ P("]") ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightIndex(v._1), v._2)) |
      P(P("(") ~ argumentExpressionList.? ~ P(")") ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightArgs(v._1), v._2)) |
      P(P(".") ~ identifier ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightDot(v._1), v._2)) |
      P(P("->") ~ identifier ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightArrow(v._1), v._2)) |
      P(P("++") ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightPlusPlus(), v)) |
      P(P("--") ~ postfixExpressionR).map(v => PostfixRight2(PostfixRightMinusMinus(), v)) |
      P("").map(v => PostfixRight2(Empty(), null))

  // Can't figure this one out
  // P(P("(") ~ typeName ~ P(")") ~ P("{") ~ initializerList ~ P(",").? ~ P("}"))
  lazy val argumentExpressionList: Parser[ArgumentExpressionList] =
  P(assignmentExpression ~ P(P(",") ~ assignmentExpression).rep(0)).map(v =>
    ArgumentExpressionList((v._1 +: v._2).toList))

  lazy val unaryExpression: Parser[Expression] =
    P(P(P("++") ~ unaryExpression).map(v => UnaryExpressionPlusPlus(v)) |
      P(P("--") ~ unaryExpression).map(v => UnaryExpressionMinusMinus(v)) |
      P(unaryOperator.! ~ castExpression).map(v => UnaryExpressionCast(v._1.charAt(0), v._2)) |
      P(P("sizeof") ~ unaryExpression).map(v => UnaryExpressionSizeOf(v)) |
      P(P("sizeof") ~ P("(") ~ typeName ~ P(")")).map(v => UnaryExpressionSizeOfType(v)) |
      P(P("_Alignof") ~ P("(") ~ typeName ~ P(")")).map(v => UnaryExpressionAlignOf(v)) |
      postfixExpression).opaque("unaryExpression")

  lazy val unaryOperator = CharIn("&*+-~!")

  lazy val castExpression: Parser[Expression] =
    P(P(P("(") ~ typeName ~ P(")") ~ castExpression).map(v => CastExpression(v._1, v._2)) |
      unaryExpression).opaque("castExpression")

  lazy val multiplicativeExpression: Parser[Expression] =
    P(castExpression ~ multiplicativeExpressionHelper).map(v => binary(v._1, v._2)).opaque("multiplicativeExpression")
  lazy val multiplicativeExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("*").! ~ multiplicativeExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P("/").! ~ multiplicativeExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P("%").! ~ multiplicativeExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  // These methods help us glue full expressions back together, after they had to be split apart during left recursion removal
  private def binary(left: Expression, right: BinaryOpBuildWrap): Expression = {
    right.op match {
      case "*"      => ExpressionMultiply(left, right.next)
      case "/"      => ExpressionDivision(left, right.next)
      case "%"      => ExpressionMod(left, right.next)
      case "+"      => ExpressionAdd(left, right.next)
      case "-"      => ExpressionMinus(left, right.next)
      case "<<"     => ExpressionLeftShift(left, right.next)
      case ">>"     => ExpressionRightShift(left, right.next)
      case "<"      => ExpressionLessThan(left, right.next)
      case ">"      => ExpressionGreaterThan(left, right.next)
      case "<="     => ExpressionLessThanOrEqual(left, right.next)
      case ">="     => ExpressionGreaterThanOrEqual(left, right.next)
      case "=="     => ExpressionEquals(left, right.next)
      case "!="     => ExpressionNotEquals(left, right.next)
      case "&"      => ExpressionAnd(left, right.next)
      case "^"      => ExpressionXOr(left, right.next)
      case "|"      => ExpressionInclusiveOr(left, right.next)
      case "&&"     => ExpressionLogicalAnd(left, right.next)
      case "||"     => ExpressionLogicalOr(left, right.next)
      case "="      => ExpressionAssignment(left, right.next)
      case "*="     => ExpressionAssignment(left, ExpressionMultiply(left, right.next))
      case "/="     => ExpressionAssignment(left, ExpressionDivision(left, right.next))
      case "%="     => ExpressionAssignment(left, ExpressionMod(left, right.next))
      case "+="     => ExpressionAssignment(left, ExpressionAdd(left, right.next))
      case "-="     => ExpressionAssignment(left, ExpressionMinus(left, right.next))
      case "<<="    => ExpressionAssignment(left, ExpressionLeftShift(left, right.next))
      case ">>="    => ExpressionAssignment(left, ExpressionRightShift(left, right.next))
      case "&="     => ExpressionAssignment(left, ExpressionAnd(left, right.next))
      case "^="     => ExpressionAssignment(left, ExpressionXOr(left, right.next))
      case "|="     => ExpressionAssignment(left, ExpressionInclusiveOr(left, right.next))
      case ","      => ExpressionComma(left, right.next)
      case "" | " " => left
      case _        =>
        assert(false)
        left
    }
  }

  private def ternary(left: Expression, right: TernaryOpBuildWrap): Expression = {
    right.op1 match {
      case "?" => ExpressionConditional(left, right.v1, right.v2)
      case _   => left
    }
  }

  lazy val additiveExpression: Parser[Expression] =
    (multiplicativeExpression ~ additiveExpressionHelper).map(v => binary(v._1, v._2)).opaque("additiveExpression")
  lazy val additiveExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("+").! ~ additiveExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P("-").! ~ additiveExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  lazy val shiftExpression: Parser[Expression] =
    (additiveExpression ~ shiftExpressionHelper).map(v => binary(v._1, v._2)).opaque("shiftExpression")
  lazy val shiftExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("<<").! ~ shiftExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P(">>").! ~ shiftExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  lazy val relationalExpression: Parser[Expression] =
    (shiftExpression ~ relationalExpressionHelper).map(v => binary(v._1, v._2)).opaque("relationalExpression")
  lazy val relationalExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("<").! ~ relationalExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P(">").! ~ relationalExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P("<=").! ~ relationalExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P(">=").! ~ relationalExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  lazy val equalityExpression: Parser[Expression] =
    (relationalExpression ~ equalityExpressionHelper).map(v => binary(v._1, v._2)).opaque("equalityExpression")
  lazy val equalityExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("==").! ~ equalityExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P(P("!=").! ~ equalityExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  lazy val ANDExpression: Parser[Expression] =
    (equalityExpression ~ ANDExpressionHelper).map(v => binary(v._1, v._2)).opaque("ANDExpression")
  lazy val ANDExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("&").! ~ ANDExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  lazy val exclusiveORExpression: Parser[Expression] =
    (ANDExpression ~ exclusiveORExpressionHelper).map(v => binary(v._1, v._2))
  lazy val exclusiveORExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("^").! ~ exclusiveORExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  lazy val inclusiveORExpression: Parser[Expression] =
    (exclusiveORExpression ~ inclusiveORExpressionHelper).map(v => binary(v._1, v._2))
  lazy val inclusiveORExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("|").! ~ inclusiveORExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  lazy val logicalANDExpression: Parser[Expression] =
    (inclusiveORExpression ~ logicalANDExpressionHelper).map(v => binary(v._1, v._2))
  lazy val logicalANDExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("&&").! ~ logicalANDExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  lazy val logicalORExpression: Parser[Expression] =
    (logicalANDExpression ~ logicalORExpressionHelper).map(v => binary(v._1, v._2)).opaque("logicalORExpression")
  lazy val logicalORExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(P("||").! ~ logicalORExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))

  lazy val conditionalExpression: Parser[Expression] =
    (logicalORExpression ~ conditionalExpressionHelper).map(v => ternary(v._1, v._2)).opaque("conditionalExpression")
  lazy val conditionalExpressionHelper: Parser[TernaryOpBuildWrap] =
    P(P("?") ~ expression ~ P(":") ~ conditionalExpression).map(v => TernaryOpBuildWrap("?", ":", v._1, v._2)) |
      P("").map(v => TernaryOpBuildWrap(" ", " ", null, null))

  lazy val assignmentExpression: Parser[Expression] =
    (conditionalExpression ~ assignmentExpressionHelper).map(v => binary(v._1, v._2)).opaque("assignmentExpression")
  lazy val assignmentExpressionHelper: Parser[BinaryOpBuildWrap] =
    P(assignmentOperator.! ~ assignmentExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))
  lazy val assignmentOperator = P("=") | P("*=") | P("/=") | P("%=") | P("+=") | P("-=") | P("<<=") | P(">>=") | P("&=") | P("^=") | P("|=")

  lazy val expression: Parser[Expression] =
    (assignmentExpression ~ expressionHelper).map(v => binary(v._1, v._2)).opaque("expression")
  lazy val expressionHelper: Parser[BinaryOpBuildWrap] =
    P(P(P(",").! ~ assignmentExpression).map(v => BinaryOpBuildWrap(v._1, v._2)) |
      P("").map(v => BinaryOpBuildWrap(" ", null))).opaque("expressionHelper")

  lazy val constantExpression: Parser[Expression] = conditionalExpression

  // int hello;
  lazy val declaration: Parser[Declaration] = P(P(P(declarationSpecifiers ~ initDeclaratorList.? ~ P(";")).map(v => SimpleDeclaration(v._1, v._2)) |
    static_assertDeclaration)).opaque("declaration")
  lazy val declarationSpecifier: Parser[DeclarationSpecifier] =
    P(storageClassSpecifier | typeSpecifier | typeQualifier | functionSpecifier | alignmentSpecifier).opaque("declarationSpecifier")
  lazy val declarationSpecifiers: Parser[DeclarationSpecifiers] = declarationSpecifier.rep(1).map(v => DeclarationSpecifiers(v.toList)).opaque("declarationSpecifiers")

  lazy val initDeclaratorList: Parser[Seq[InitDeclarator]] = P(initDeclarator ~ P(P(",") ~ initDeclarator).rep(0)).opaque("initDeclarationList").map(v => (v._1 +: v._2).toList)
  lazy val initDeclarator: Parser[InitDeclarator] =
    P(declarator ~ P("=") ~ initializer).map(v => DeclaratorWithInit(v._1, v._2)) |
      declarator.map(DeclaratorEmpty)

  lazy val storageClassSpecifier: Parser[StorageClassSpecifier] =
    P(P("typedef") | P("extern") | P("static") | P("_Thread_local") | P("auto") | P("register")).!.map(StorageClassSpecifier)
  //  lazy val typeSpecifier: Parser[TypeSpecifier] = (P("void") | P("char") | P("short") | P("int") | P("long") | P("float") | P("double") | P("signed") | P("unsigned") | P("_Bool") | P("_Complex") | atomicTypeSpecifier | structOrUnionSpecifier | enumSpecifier | typedefName).!.map(TypeSpecifier)
  // TODO
  lazy val typeSpecifier: Parser[TypeSpecifier] = (P("void") | P("char") | P("short") | P("int") | P("long") | P("float") | P("double") | P("signed") | P("unsigned") | P("_Bool") | P("_Complex") | atomicTypeSpecifier | structOrUnionSpecifier | enumSpecifier).!.map(TypeSpecifier)
  lazy val structOrUnionSpecifier = P(structOrUnion ~ identifier.? ~ P("{") ~ structDeclarationList ~ P("}")) |
    P(structOrUnion ~ identifier)
  lazy val structOrUnion = P("struct") | P("union")
  lazy val structDeclarationList: Parser[Any] = structDeclaration |
    P(structDeclarationList ~ structDeclaration)
  lazy val structDeclaration = P(specifierQualifierList ~ structDeclaratorList.? ~ P(";")) |
    static_assertDeclaration
  lazy val specifierQualifierList: Parser[Any] = P(typeSpecifier ~ specifierQualifierList.?) |
    P(typeQualifier ~ specifierQualifierList.?)
  lazy val structDeclaratorList: Parser[Any] = structDeclarator |
    P(structDeclaratorList ~ P(",") ~ structDeclarator)
  lazy val structDeclarator = declarator |
    P(declarator.? ~ P(":") ~ constantExpression)

  lazy val enumSpecifier = P(P("enum") ~ identifier.? ~ P("{") ~ enumeratorList ~ P("}") ~ P(",").?) |
    P(P("enum") ~ identifier)
  lazy val enumeratorList: Parser[Any] = enumerator |
    P(enumeratorList ~ P(",") ~ enumerator)
  lazy val enumerator = enumerationConstant |
    P(enumerationConstant ~ P("=") ~ constantExpression)

  lazy val atomicTypeSpecifier = P("_Atomic") ~ P("(") ~ typeName ~ P(")")
  lazy val typeQualifier: Parser[TypeQualifier] = (P("const") | P("restrict") | P("volatile") | P("_Atomic")).!.map(TypeQualifier)
  lazy val functionSpecifier: Parser[FunctionSpecifier] = (P("inline") | P("_Noreturn")).!.map(FunctionSpecifier)
  lazy val alignmentSpecifier: Parser[AlignmentSpecifier] = P("_Alignas").!.map(AlignmentSpecifier) |
    P(P("(") ~ typeName ~ P(")")).!.map(AlignmentSpecifier) |
    P(P("_Alignas") ~ P("(") ~ constantExpression ~ P(")")).!.map(AlignmentSpecifier)

  // "main(int argc)"
  // "someFunc(hello, world)"
  lazy val declarator: Parser[Declarator] = (pointer.!.? ~ directDeclarator).opaque("declarator").map(v => Declarator(v._1, v._2))
  lazy val directDeclarator: Parser[DirectDeclarator] = P(P(P("(") ~ declarator ~ P(")")).map(DDBracketed) |
    (identifier ~ directDeclaratorHelper).map(v => {
      directDeclaratorRecurse(v._1, v._2)
    })).opaque("directDeclarator")
  lazy val directDeclaratorHelper: Parser[DDBuild2] =
    P(
      P(P("[") ~ typeQualifierList.? ~ assignmentExpression.? ~ P("]") ~ directDeclaratorHelper).map(v => DDBuild2(DDBuildTypeQualifierListAssignment(v._1, v._2), v._3)) |
        P(P("[") ~ P("static") ~ typeQualifierList.? ~ assignmentExpression ~ P("]") ~ directDeclaratorHelper).map(v => DDBuild2(DDBuildTypeQualifierListAssignment(v._1, Some(v._2)), v._3)) |
        P(P("[") ~ typeQualifierList ~ P("static") ~ assignmentExpression ~ P("]") ~ directDeclaratorHelper).map(v => DDBuild2(DDBuildTypeQualifierListAssignment(Some(v._1), Some(v._2)), v._3)) |
        P(P("[") ~ typeQualifierList.? ~ P("*") ~ P("]") ~ directDeclaratorHelper).map(v => DDBuild2(DDBuildTypeQualifierList(v._1), v._2)) |
        P(P("(") ~ parameterTypeList ~ P(")") ~ directDeclaratorHelper).map(v => DDBuild2(DDBuildParameterTypeList(v._1), v._2)) |
        P(P("(") ~ identifierList.? ~ P(")") ~ directDeclaratorHelper).map(v => DDBuild2(DDBuildIdentifierList(v._1), v._2)) |
        P("").map(v => DDBuild2(Empty(), null))
    ).opaque("directDeclaratorHelper")

  private def directDeclaratorRecurse(identifier: Identifier, right: DDBuild2): DirectDeclarator = {
    // Just ignore right for now, don't know how to handle it
    directDeclaratorMerge(identifier, right.me)
  }

  def directDeclaratorMerge(left: Identifier, right: DDBuild): DirectDeclarator = {
    right match {
      case v: DDBuildParameterTypeList => FunctionDeclaration(left, v.v)
      case v: Empty                    => DirectDeclaratorOnly(left)
      case _                           =>
        assert(false, s"Cannot handle DDBuild $right yet")
        null
    }
  }


  lazy val pointer: Parser[Any] = P("*") ~ typeQualifierList.? |
    P(P("*") ~ typeQualifierList.? ~ pointer)
  lazy val typeQualifierList = typeQualifier.rep(1)

  lazy val parameterTypeList: Parser[ParameterTypeList] =
    (parameterList ~ P(P(",") ~ P("...")).!.?).opaque("parameterTypeList").map(v =>
      ParameterTypeList(v._1, v._2.isDefined))
  lazy val parameterList: Parser[Seq[ParameterDeclaration]] =
    (parameterDeclaration ~ P(P(",") ~ parameterDeclaration).rep(0)).opaque("parameterList").map(v => (v._1 +: v._2).toList)
  lazy val parameterDeclaration: Parser[ParameterDeclaration] = P(declarationSpecifiers ~ declarator).map(v => ParameterDeclarationDeclarator(v._1, v._2))
  // TODO need but directAbstractDeclarator will be a pain
  //    P(declarationSpecifiers ~ abstractDeclarator.?).map(v => ParameterDeclarationAbstractDeclarator(v._1, v._2))

  lazy val identifierList: Parser[Seq[Identifier]] =
    (identifier ~ (P(",") ~ identifier).rep(0)).opaque("identifierList").map(v => (v._1 +: v._2).toList)

  // TODO This needs to go back
  //  lazy val typeName: Parser[TypeName] = P(specifierQualifierList ~ abstractDeclarator.?).!.map(TypeName)
  lazy val typeName: Parser[TypeName] = P(specifierQualifierList).!.map(TypeName)
  lazy val abstractDeclarator: Parser[Any] = pointer |
    P(pointer.? ~ directAbstractDeclarator)
  lazy val directAbstractDeclarator: Parser[Any] = P(P("(") ~ abstractDeclarator ~ P(")")) |
    P(directAbstractDeclarator.? ~ P("[") ~ typeQualifierList.?) ~ P(assignmentExpression.? ~ P("]")) |
    P(directAbstractDeclarator.? ~ P("[") ~ P("static") ~ typeQualifierList.?) ~ P(assignmentExpression ~ P("]")) |
    P(directAbstractDeclarator.? ~ P("[") ~ typeQualifierList ~ P("static")) ~ P(assignmentExpression ~ P("]")) |
    P(directAbstractDeclarator.? ~ P("[") ~ P("*") ~ P("]")) |
    P(directAbstractDeclarator.? ~ P("(") ~ parameterTypeList.? ~ P(")"))
  lazy val typedefName = identifier
  lazy val initializer: Parser[Initializer] = assignmentExpression.map(InitializerSimple)
  // TODO
  //    P(P("{") ~ initializerList ~ P(",").? ~ P("}"))
  lazy val initializerList: Parser[Any] = designation.? ~ initializer |
    P(initializerList ~ P(",") ~ designation.? ~ initializer)
  lazy val designation = designatorList ~ P("=")
  lazy val designatorList: Parser[Any] = designator |
    P(designatorList ~ designator)
  lazy val designator = P("[") ~ constantExpression ~ P("]") |
    P(P(".") ~ identifier)
  lazy val static_assertDeclaration =
    P(P("_Static_assert") ~ P("(") ~ constantExpression ~ P(",") ~ stringLiteral ~ P(")") ~ P(";"))
      .map(v => StaticAssertDeclaration(v._1, v._2))

  val statement: Parser[Statement] = P(labeledStatement) |
    P(compoundStatement) |
    P(expressionStatement) |
    P(selectionStatement) |
    P(iterationStatement) |
    P(jumpStatement)
  val labeledStatement: Parser[LabelledStatement] = (identifier ~ P(":") ~ statement).map(v => LabelledLabel(v._1, v._2))
  P(P("case") ~ constantExpression ~ P(":") ~ statement).map(v => LabelledCase(v._1, v._2)) |
    P(P("default") ~ P(":") ~ statement).map(v => LabelledDefault(v))
  val compoundStatement: Parser[CompoundStatement] = P("{") ~ blockItemList.?.opaque("compoundStatement").map(v => CompoundStatement(v.getOrElse(List()))) ~ P("}")
  lazy val blockItemList: Parser[Seq[BlockItem]] = blockItem.rep(1).opaque("blockItemList").map(v => v.toList)
  lazy val blockItem: Parser[BlockItem] = declaration | statement
  val expressionStatement: Parser[Statement] = (expression.? ~ P(";")).map(v => if (v.isDefined) ExpressionStatement(v.get) else ExpressionEmptyStatement())
  val selectionStatement: Parser[SelectionStatement] = P(P("if") ~ P("(") ~ expression ~ P(")") ~ statement).map(v => SelectionIf(v._1, v._2)) |
    P(P("if") ~ P("(") ~ expression ~ P(")") ~ statement ~ P("else") ~ statement).map(v => SelectionIfElse(v._1, v._2, v._3)) |
    P(P("switch") ~ P("(") ~ expression ~ P(")") ~ statement).map(v => SelectionSwitch(v._1, v._2))
  val iterationStatement: Parser[IterationStatement] = P(P("while") ~ P("(") ~ expression ~ P(")") ~ statement).map(v => IterationWhile(v._1, v._2)) |
    P(P("do") ~ statement ~ P("while") ~ P("(") ~ expression ~ P(")") ~ P(";")).map(v => IterationDoWhile(v._2, v._1)) |
    P(P("for") ~ P("(") ~ expression.? ~ P(";") ~ expression.? ~ P(";") ~ expression.? ~ P(")") ~ statement).map(v => IterationFor1(v._1, v._2, v._3, v._4))
  // TODO when declaration read
  //    P(P("for") ~ P("(") ~ declaration ~ expression.? ~ P(";") ~ expression.? ~ P(")") ~ statement).map(v => IterationFor2(v._1, v._2, v._3))
  val jumpStatement: Parser[JumpStatement] = P(P("goto") ~ identifier ~ P(";")).map(v => Goto(v)) |
    P(P("continue") ~ P(";")).map(v => Continue()) |
    P(P("break") ~ P(";")).map(v => Break()) |
    P(P("return") ~ expression.? ~ P(";")).map(v => Return(v))

  //  A.2.4 External definitions
  val declarationList: Parser[DeclarationList] = declaration.rep(1).map(v => DeclarationList(v)).opaque("declarationList")
  val functionDefinition: Parser[FunctionDefinition] =
    (declarationSpecifiers ~ declarator ~ declarationList.? ~ compoundStatement).opaque("functionDefinition").map(v =>
      FunctionDefinition(v._1, v._2, v._3, v._4))
  val externalDeclaration: Parser[ExternalDeclaration] = functionDefinition | declaration
  val translationUnit: Parser[TranslationUnit] = externalDeclaration.rep(1).map(v => TranslationUnit(v.toList))
  val top = translationUnit

  // TODO only preprocessor tokens left!
  //    A.3 Preprocessing directives
  //  lazy val preprocessingFile = group.?
  //  lazy val group = groupPart
  //  group groupPart
  //  lazy val groupPart:
  //    ifSection
  //  controlLine
  //  textLine
  //  # nonDirective
  //  lazy val ifSection:
  //    ifGroup elifGroups.?elseGroup.?endifLine
  //  lazy val ifGroup:
  //  # if constantExpression newLine group.?
  //  # ifdef identifier newLine group.?
  //  # ifndef identifier newLine group.?
  //  lazy val elifGroups = elifGroup
  //  elifGroups elifGroup
  //  lazy val elifGroup:
  //  # elif constantExpression newLine group.?
  //  lazy val elseGroup:
  //  # else newLine group.?
  //  lazy val endifLine:
  //  # endif newLine
  //  lazy val controlLine:
  //  # include ppTokens newLine
  //  # define identifier replacementList newLine
  //  # define identifier lparen identifierList.?)
  //  replacementList newLine
  //  # define identifier lparen ... ) replacementList newLine
  //  # define identifier lparen identifierList , ... )
  //  replacementList newLine
  //  # undef identifier newLine
  //  # line ppTokens newLine
  //  # error ppTokens.?newLine
  //  # pragma ppTokens.?newLine
  //  # newLine
  //  lazy val textLine = ppTokens.? ~ newLine
  //  lazy val nonDirective = ppTokens !newLine
  //  lazy val lparen = a ( character not immediately preceded by whiteSpace
  //  lazy val replacementList = ppTokens.?
  //  lazy val ppTokens = preprocessingToken
  //  ppTokens preprocessingToken
  //  lazy val newLine = P("\n")

  def parse(in: String): CParseResult = {
    CParseResult.wrap(top.parse(in))
  }

  def parseSnippet(in: String): CParseResult = {
    CParseResult.wrap(blockItemList.parse(in))
  }
}
