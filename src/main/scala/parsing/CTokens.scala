package parsing

/** These tokens are everything used in the abstract syntax tree for C.
  * They map pretty much one-to-one with the spec // https://port70.net/~nsz/c/c11/n1570.html#A
  */
case class Nondigit(v: Char)
case class Digit(v: Char)
case class DecimalConstant(v: Int)
case class Exponent(v: Int) {
  def calc(): Float = Math.pow(10, v).toFloat
}
sealed trait Token
sealed trait Constant extends Token with Expression with PPToken
case class IntConstant(v: Int) extends Constant
case class FloatConstant(v: Float) extends Constant
case class EnumerationConstant(v: String) extends Constant
case class CharacterConstant(v: String) extends Constant with PPToken
case class OctalConstant(v: Int)
case class HexConstant(v: Int)
case class HexDigit(v: Char)
case class HexQuad(v1: HexDigit, v2: HexDigit, v3: HexDigit, v4: HexDigit)
sealed trait UniversalCharacterName
case class UniversalCharacterName1(v: HexQuad) extends UniversalCharacterName
case class UniversalCharacterName2(v1: HexQuad, v2: HexQuad) extends UniversalCharacterName
sealed trait IdentifierNondigit
case class IdentifierNondigit1(v: Nondigit) extends IdentifierNondigit
case class IdentifierNondigit2(v: UniversalCharacterName) extends IdentifierNondigit
sealed trait Expression
case class Identifier(v: String) extends Token with Expression with PPToken
case class Keyword(v: String) extends Token
case class Punctuator(v: String) extends Token with PPToken
case class StringLiteral(v: String) extends Token with Expression with PPToken
case class HeaderName(v: String, angularBrackets: Boolean) extends Token with PPToken
//case class GenericSelection() extends Expression
case class PostfixExpressionIndex(v1: Expression, v2: Expression) extends Expression
case class PostfixExpressionDot(v1: Expression, v2: Expression) extends Expression
case class PostfixExpressionPlusPlus(v1: Expression) extends Expression
case class PostfixExpressionMinusMinus(v1: Expression) extends Expression
case class PostfixExpressionArrow(v1: Expression, v2: Expression) extends Expression
case class PostfixExpressionArgs(v1: Expression, v2: Option[ArgumentExpressionList]) extends Expression
case class PostfixExpressionSimple(v1: Expression) extends Expression

sealed trait UnaryExpression extends Expression

case class ArgumentExpressionList(v: Seq[Expression]) extends Expression
case class UnaryExpressionPlusPlus(v: Expression) extends Expression
case class UnaryExpressionMinusMinus(v: Expression) extends Expression
case class UnaryExpressionCast(v: Char, v2: Expression) extends Expression
case class UnaryExpressionSizeOf(v: Expression) extends Expression
case class UnaryExpressionSizeOfType(v: TypeName) extends Expression
case class UnaryExpressionAlignOf(v: TypeName) extends Expression
//case class TypeName(v: String, abs: Option[AbstractDeclarator])
case class TypeName(v: String)
case class CastExpression(v: TypeName, v2: Expression) extends Expression
case class ExpressionMultiply(v1: Expression, v2: Expression) extends Expression
case class ExpressionDivision(v1: Expression, v2: Expression) extends Expression
case class ExpressionMod(v1: Expression, v2: Expression) extends Expression
case class ExpressionAdd(v1: Expression, v2: Expression) extends Expression
case class ExpressionMinus(v1: Expression, v2: Expression) extends Expression
case class ExpressionLeftShift(v1: Expression, v2: Expression) extends Expression
case class ExpressionRightShift(v1: Expression, v2: Expression) extends Expression
case class ExpressionLessThan(v1: Expression, v2: Expression) extends Expression
case class ExpressionGreaterThan(v1: Expression, v2: Expression) extends Expression
case class ExpressionLessThanOrEqual(v1: Expression, v2: Expression) extends Expression
case class ExpressionGreaterThanOrEqual(v1: Expression, v2: Expression) extends Expression
case class ExpressionEquals(v1: Expression, v2: Expression) extends Expression // ==
case class ExpressionNotEquals(v1: Expression, v2: Expression) extends Expression // !=
case class ExpressionAnd(v1: Expression, v2: Expression) extends Expression // &
case class ExpressionXOr(v1: Expression, v2: Expression) extends Expression // ^
case class ExpressionInclusiveOr(v1: Expression, v2: Expression) extends Expression // |
case class ExpressionLogicalAnd(v1: Expression, v2: Expression) extends Expression // &&
case class ExpressionLogicalOr(v1: Expression, v2: Expression) extends Expression // ||
case class ExpressionConditional(v1: Expression, v2: Expression, v3: Expression) extends Expression // ?:
case class ExpressionComma(v1: Expression, v2: Expression) extends Expression // ,
case class ExpressionAssignment(v1: Expression, v2: Expression) extends Expression // ,

sealed trait Statement extends BlockItem
sealed trait JumpStatement extends Statement
case class Goto(v: Identifier) extends JumpStatement
case class Continue() extends JumpStatement
case class Break() extends JumpStatement
case class Return(v: Option[Expression]) extends JumpStatement
case class ExpressionStatement(expression: Expression) extends Statement
case class ExpressionEmptyStatement() extends Statement
sealed trait SelectionStatement extends Statement
case class SelectionIf(v1: Expression, v2: Statement) extends SelectionStatement
case class SelectionIfElse(v1: Expression, v2: Statement, v3: Statement) extends SelectionStatement
case class SelectionSwitch(v1: Expression, v2: Statement) extends SelectionStatement

sealed trait IterationStatement extends Statement
case class IterationWhile(v1: Expression, v2: Statement) extends IterationStatement
case class IterationDoWhile(v1: Expression, v2: Statement) extends IterationStatement
case class IterationFor1(v1: Option[Expression], v2: Option[Expression], v3: Option[Expression], v4: Statement) extends IterationStatement
case class IterationFor2(v: Declaration, v1: Option[Expression], v2: Option[Expression], v3: Statement) extends IterationStatement

sealed trait LabelledStatement extends Statement
case class LabelledLabel(v1: Identifier, v2: Statement) extends LabelledStatement
case class LabelledCase(v1: Expression, v2: Statement) extends LabelledStatement
case class LabelledDefault(v2: Statement) extends LabelledStatement

// >>int hello;<<
sealed trait Declaration extends ExternalDeclaration with BlockItem
case class SimpleDeclaration(spec: DeclarationSpecifiers, init: Option[Seq[InitDeclarator]]) extends Declaration
case class StaticAssertDeclaration(v1: Expression, v2: StringLiteral) extends Declaration
//case class StatementDeclaration(v: Declaration) extends Statement
case class CompoundStatement(v: Seq[BlockItem]) extends Statement
//case class BlockItemList(v: Seq[BlockItem])
sealed trait BlockItem

sealed trait InitDeclarator
// int >>hello<<;
case class DeclaratorEmpty(declarator: Declarator) extends InitDeclarator
// int >>hello=3<<;
case class DeclaratorWithInit(declarator: Declarator, init: Initializer) extends InitDeclarator

sealed trait Initializer
// int hello=>>3<<;
case class InitializerSimple(exp: Expression) extends Initializer
// int hello[] = >>{1,2,3]<<;
case class InitializerList(exp: Expression) extends Initializer

// >>int<< hello;
sealed trait DeclarationSpecifier {
  val v: String
}
case class DeclarationSpecifiers(v: Seq[DeclarationSpecifier])
case class Declarator(pointer: Option[String], v: DirectDeclarator)
case class StorageClassSpecifier(v: String) extends DeclarationSpecifier
sealed trait TypeSpecifier extends DeclarationSpecifier
case class TypeSpecifierSimple(v: String) extends TypeSpecifier
case class TypeQualifier(v: String) extends DeclarationSpecifier
case class FunctionSpecifier(v: String) extends DeclarationSpecifier
case class AlignmentSpecifier(v: String) extends DeclarationSpecifier

case class AbstractDeclarator(v: String)

case class TranslationUnit(v: Seq[Top])
sealed trait ExternalDeclaration extends Top
case class FunctionDefinition(spec: DeclarationSpecifiers, dec: Declarator, decs: Option[DeclarationList], v: CompoundStatement) extends ExternalDeclaration
case class DeclarationList(v: Seq[Declaration])

case class ParameterTypeList(v: Seq[ParameterDeclaration], ellipses: Boolean)
sealed trait ParameterDeclaration
case class ParameterDeclarationDeclarator(v: DeclarationSpecifiers, v2: Declarator) extends ParameterDeclaration

sealed trait DirectDeclarator

case class DirectDeclaratorOnly(v: Identifier) extends DirectDeclarator
case class DDBracketed(declarator: Declarator) extends DirectDeclarator
case class FunctionDeclaration(name: Identifier, params: ParameterTypeList) extends DirectDeclarator

case class PreprocessingFile(v: Group) extends Top
case class Group(v: Seq[GroupPart])
sealed trait GroupPart
case class IfSection(ifGroup: IfGroup, elif: Option[Seq[ElifGroup]], elseGroup: Option[ElseGroup], endif: EndifLine) extends GroupPart
sealed trait IfGroup
case class If(exp: Expression, group: Option[Group]) extends IfGroup
case class IfDef(ident: Identifier, group: Option[Group]) extends IfGroup
case class IfNDef(ident: Identifier, group: Option[Group]) extends IfGroup
case class ElifGroup(exp: Expression, group: Option[Group])
case class EndifLine()
case class ElseGroup(group: Option[Group])
sealed trait ControlLine extends GroupPart
case class Include(v: Seq[PPToken]) extends ControlLine
case class Define(ident: Identifier, v: ReplacementList) extends ControlLine
case class Define2(ident: Identifier, v: Option[Seq[Identifier]], v2: ReplacementList) extends ControlLine
case class Define3(ident: Identifier, v: ReplacementList) extends ControlLine
case class Define4(ident: Identifier, v: Seq[Identifier], v2: ReplacementList) extends ControlLine
case class Undef(ident: Identifier) extends ControlLine
case class Line(v: Seq[PPToken]) extends ControlLine
case class Error(v: Option[Seq[PPToken]]) extends ControlLine
case class Pragma(v: Option[Seq[PPToken]]) extends ControlLine
case class ControlLineEmpty() extends ControlLine
case class ReplacementList(v: Option[Seq[PPToken]])
sealed trait PPToken
//case class PPToken(v: String)
case class TextLine(pp: Option[Seq[PPToken]]) extends GroupPart
case class NonDirective(pp: Seq[PPToken]) extends GroupPart

sealed trait StructDeclaractor
case class StructDeclaractor1(v: Declarator) extends StructDeclaractor
case class StructDeclaractor2(v: Option[Declarator], exp: Expression) extends StructDeclaractor
case class StructDeclaratorList(v: Seq[StructDeclaractor])
case class StructDeclaration(v: Seq[DeclarationSpecifier], v2: Option[StructDeclaratorList])
case class StructOrUnionSpecifier(isStruct: Boolean, id: Option[Identifier], v2: Seq[StructDeclaration]) extends DeclarationSpecifier with TypeSpecifier {
  val v = toString
}

// The C grammar doesn't have a top level that can either be regular C (translation-unit) or the preprocessor (preprocessing-file).
// Presumably it's because they're handled by separate tools.  For convenience, translation-unit gets redefined slightly
// to take preprocessor statements too.
sealed trait Top
//case class CFile(v: Seq[Top])
//case class CFile(v: TranslationUnit)

