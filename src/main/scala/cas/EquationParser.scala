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
    (CharsWhile(_.isLetter).rep(1).! ~ ("_" ~ CharsWhile(_.isLetterOrDigit).rep(1).!).?).map({
      case (x, None) => Variable(x)
      case (x, Some(subscript)) => Variable(x + "_" + subscript)
    }))
  lazy val parens: P[Expression[String]] = P( "(" ~/ expr0 ~ ")" )
//  lazy val functionCall: P[Expression[String]] = P(CharsWhile(_.isLetter).rep(1).! ~ "(" ~ expr.rep(sep=",") ~ ")").map({
//    case (name: String, args: Seq[Expression[String]]) => SpecialFunction(name, args.toList)
//  })
  lazy val functionCall: P[Expression[String]] = P(CharsWhile(_.isLetter).rep(1).! ~ "(" ~ expr0.rep(sep=",") ~ ")").map({
    case (name: String, args: Seq[Expression[_]]) => SpecialFunction(name, args.toList)
  })

  val atom: P[Expression[String]] = P( number | functionCall | variable | parens )

  val expr2: P[Expression[String]] = P("-".!.? ~ (atom ~ "**" ~ atom | atom)).map({
    case (mbMinus: Option[String], x: Expression[_]) =>
       x.asInstanceOf[Expression[String]] * (if (mbMinus.isDefined) -1 else 1)
    case (mbMinus: Option[String], (x: Expression[_], y: Expression[_])) =>
      (Expression.makePower(x.asInstanceOf[Expression[String]], y.asInstanceOf[Expression[String]])
        * (if (mbMinus.isDefined) -1 else 1))
  })
  val expr1: P[Expression[String]] = P( expr2 ~ (CharIn("*/").! ~/ expr2).rep ).map(eval)
  val expr0: P[Expression[String]] = P( expr1 ~ (CharIn("+-").! ~/ expr1).rep ).map(eval)
  val expr: P[Expression[String]]   = P( " ".rep ~ expr0 ~ " ".rep ~ End )
}
