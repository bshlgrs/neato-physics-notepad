package cas

import fastparse.WhitespaceApi
import fastparse.core.Parsed
import workspace.CustomEquation

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("Gem.EquationParser")
object EquationParser {
  @JSExport
  def parseEquationJs(equationString: String): CustomEquation = parseEquation(equationString).orNull

  def parseEquation(equationString: String): Option[CustomEquation] = for {
    list: List[String] <- Option(equationString.split('=').toList)
    if list.size == 2
    lhs <- parseExpression(list(0))
    rhs <- parseExpression(list(1))
  } yield CustomEquation(lhs, rhs)

  def parseExpression(expressionString: String): Option[Expression[String]] = expr.parse(expressionString) match {
    case Parsed.Success(expression, _) => Some(expression)
    case _ => None
  }

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import fastparse.noApi._
  import White._
  def eval(tree: (Expression[String], Seq[(String, Expression[String])])): Expression[String] = {
    val (base, ops) = tree
    ops.foldLeft(base){
      case (left, (op, right)) => op match {
        case "+" => left + right case "-" => left - right
        case "*" => left * right case "/" => left / right
      }
    }
  }

  val number: P[Expression[String]] = P( CharIn('0'to'9').rep(1).!.map(str => RationalNumber[String](str.toInt)))
  val variable: P[Expression[String]] = P(
    (CharIn('a' to 'z', 'A' to 'Z').rep(1).! ~ ("_" ~ CharIn('a' to 'z', 'A' to 'Z').rep(1).!).?).map({
      case (x, None) => Variable(x)
      case (x, Some(subscript)) => Variable(x + "_" + subscript)
    }))
  lazy val parens: P[Expression[String]] = P( "(" ~/ addSub ~ ")" )
  val atom: P[Expression[String]] = P( number | variable | parens )

  val power: P[Expression[String]] = P(atom ~ "**" ~ atom | atom).map({
    case x: Expression[_] => x.asInstanceOf[Expression[String]]
    case (x: Expression[_], y: Expression[_]) => Expression.makePower(x.asInstanceOf[Expression[String]], y.asInstanceOf[Expression[String]])
  })
  val divMul: P[Expression[String]] = P( power ~ (CharIn("*/").! ~/ power).rep ).map(eval)
  val addSub: P[Expression[String]] = P( divMul ~ (CharIn("+-").! ~/ divMul).rep ).map(eval)
  val expr: P[Expression[String]]   = P( " ".rep ~ addSub ~ " ".rep ~ End )
}
