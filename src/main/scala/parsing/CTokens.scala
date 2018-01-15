package parsing

case class Nondigit(v: Char)
case class Digit(v: Char)
case class DecimalConstant(v: Int)
case class Exponent(v: Int) {
  def calc(): Float = Math.pow(10, v).toFloat
}
sealed trait Token
sealed trait Constant extends Token with Expression
case class IntConstant(v: Int) extends Constant
case class FloatConstant(v: Float) extends Constant
case class EnumerationConstant(v: String) extends Constant
case class CharacterConstant(v: String) extends Constant
case class OctalConstant(v: Int)
case class HexConstant(v: Int)
case class HexDigit(v: Char)
case class HexQuad(v1: HexDigit, v2: HexDigit, v3: HexDigit, v4: HexDigit)
sealed trait UniversalCharacterName
case class UniversalCharacterName1(v: HexQuad) extends UniversalCharacterName
case class UniversalCharacterName2(v1: HexQuad, v2: HexQuad) extends UniversalCharacterName
sealed trait IdentifierNondigit
//sealed trait PrimaryExpression
case class IdentifierNondigit1(v: Nondigit) extends IdentifierNondigit
case class IdentifierNondigit2(v: UniversalCharacterName) extends IdentifierNondigit
sealed trait Expression
case class Identifier(v: String) extends Token with Expression
case class Keyword(v: String) extends Token
case class Punctuator(v: String) extends Token
case class StringLiteral(v: String) extends Token with Expression
case class HeaderName(v: String) extends Token
case class GenericSelection() extends Expression
case class PostfixExpressionIndex(v1: Expression, v2: Expression) extends Expression
case class PostfixExpressionDot(v1: Expression, v2: Expression) extends Expression
case class PostfixExpressionPlusPlus(v1: Expression) extends Expression
case class PostfixExpressionMinusMinus(v1: Expression) extends Expression
case class PostfixExpressionArrow(v1: Expression, v2: Expression) extends Expression
case class PostfixExpressionArgs(v1: Expression, v2: Option[ArgumentExpressionList]) extends Expression
case class PostfixExpressionSimple(v1: Expression) extends Expression
case class PostfixLeft(v: Expression)
sealed trait PostfixRight
case class PostfixRightIndex(v1: Expression) extends PostfixRight
case class PostfixRightDot(v1: Expression) extends PostfixRight
case class PostfixRightPlusPlus() extends PostfixRight
case class PostfixRightMinusMinus() extends PostfixRight
case class PostfixRightArrow(v1: Expression) extends PostfixRight
case class PostfixRightArgs(v2: Option[ArgumentExpressionList]) extends PostfixRight
case class PostfixRight2(op: PostfixRight, next: PostfixRight2)
case class Empty() extends PostfixRight with MultiplicativeBuild with DDBuild

sealed trait MultiplicativeBuild
//case class MultiplicativeBuildMultiply() extends MultiplicativeBuild
case class BinaryOpBuildWrap(op: String, next: Expression)
case class BinaryOpBuildWrap2(op: String, next: BinaryOpBuildWrap2)
case class TernaryOpBuildWrap(op1: String, op2: String, v1: Expression, v2: Expression)

sealed trait UnaryExpression extends Expression
case class UnaryPlusPlus(v: Expression) extends UnaryExpression


case class ArgumentExpressionList(v: Seq[Expression]) extends Expression
case class UnaryExpressionPlusPlus(v: Expression) extends Expression
case class UnaryExpressionMinusMinus(v: Expression) extends Expression
case class UnaryExpressionCast(v: Char, v2: Expression) extends Expression
case class UnaryExpressionSizeOf(v: Expression) extends Expression
case class UnaryExpressionSizeOfType(v: TypeName) extends Expression
case class UnaryExpressionAlignOf(v: TypeName) extends Expression
//case class UnaryExpressionPlusPlus(v: TypeName) extends Expression
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
case class IterationFor2(v1: Expression, v2: Statement) extends IterationStatement

sealed trait LabelledStatement extends Statement
case class LabelledLabel(v1: Identifier, v2: Statement) extends LabelledStatement
case class LabelledCase(v1: Expression, v2: Statement) extends LabelledStatement
case class LabelledDefault(v2: Statement) extends LabelledStatement

sealed trait Declaration extends ExternalDeclaration with BlockItem
case class SimpleDeclaration(spec: DeclarationSpecifiers, init: Option[Seq[InitDeclarator]]) extends Declaration
case class StaticAssertDeclaration(v1: Expression, v2: StringLiteral) extends Declaration
case class StatementDeclaration(v: Declaration) extends Statement
case class CompoundStatement(v: Seq[BlockItem]) extends Statement
//case class BlockItemList(v: Seq[BlockItem])
sealed trait BlockItem
sealed trait InitDeclarator
case class DeclaratorEmpty(declarator: Declarator) extends InitDeclarator
case class DeclaratorWithInit(declarator: Declarator, init: Initializer) extends InitDeclarator
sealed trait Initializer
case class InitializerSimple(exp: Expression) extends Initializer
case class InitializerList(exp: Expression) extends Initializer

sealed trait DeclarationSpecifier {
  val v: String
}
case class DeclarationSpecifiers(v: Seq[DeclarationSpecifier])
case class Declarator(pointer: Option[String], v: DirectDeclarator)
case class StorageClassSpecifier(v: String) extends DeclarationSpecifier
case class TypeSpecifier(v: String) extends DeclarationSpecifier
case class TypeQualifier(v: String) extends DeclarationSpecifier
case class FunctionSpecifier(v: String) extends DeclarationSpecifier
case class AlignmentSpecifier(v: String) extends DeclarationSpecifier

case class TranslationUnit(v: Seq[ExternalDeclaration])
sealed trait ExternalDeclaration
case class FunctionDefinition(spec: DeclarationSpecifiers, dec: Declarator, decs: Option[DeclarationList], v: CompoundStatement) extends ExternalDeclaration
case class DeclarationList(v: Seq[Declaration])

case class ParameterTypeList(v: Seq[ParameterDeclaration], ellipses: Boolean)
//case class ParameterList(v: Seq[ParameterDeclaration])
sealed trait ParameterDeclaration
case class ParameterDeclarationDeclarator(v: DeclarationSpecifiers, v2: Declarator) extends ParameterDeclaration
//case class ParameterDeclarationAbstractDeclarator(v: DeclarationSpecifiers, v2: Option[AbstractDeclarator]) extends ParameterDeclaration

sealed trait DirectDeclarator
sealed trait DDBuild
case class DDBuild2(me: DDBuild, next: DDBuild2)
case class DDBuildParameterTypeList(v: ParameterTypeList) extends DDBuild
case class DDBuildIdentifierList(v: Option[Seq[Identifier]]) extends DDBuild
case class DDBuildTypeQualifierList(v: Option[Seq[TypeQualifier]]) extends DDBuild
case class DDBuildTypeQualifierListAssignment(v: Option[Seq[TypeQualifier]], v2: Option[Expression]) extends DDBuild

case class DirectDeclaratorOnly(v: Identifier) extends DirectDeclarator
case class DDBracketed(declarator: Declarator) extends DirectDeclarator
case class FunctionDeclaration(name: Identifier, params: ParameterTypeList) extends DirectDeclarator