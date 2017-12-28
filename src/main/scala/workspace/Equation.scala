package workspace

import cas.{Expression, RationalNumber, Variable}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExportAll
case class Equation(name: String,
                    expr: Expression[String],
                    display: (String => VariableSpan) => DisplayMath,
                    dimensions: Map[String, Dimension],
                    varNames: Map[String, String],
                    tags: Set[String]
                   ) {
  assert(expr.vars == dimensions.keys)
  assert(varNames.keys == dimensions.keys)

  def solve(varName: String, selfEqId: Int): Expression[VarId] = {
    // TODO: check on the `head` here
    expr.solve(varName).head.mapVariables((name) => VarId(selfEqId, name))
  }

  def exprWithEquationId(id: Int): Expression[VarId] = expr.mapVariables((name) => VarId(id, name))

  def vars: Set[String] = expr.vars

  def varNamesJs: js.Dictionary[String] = js.Dictionary(varNames.toSeq :_*)
  def dimensionsJs: js.Dictionary[Dimension] = js.Dictionary(dimensions.toSeq :_*)
}

object Equation {
  def buildQuickly(name: String,
                   lhs: (String, String, Dimension),
                   rhsVars: Map[String, (Int, String, Dimension)],
                   tags: String,
                   constant: RationalNumber[String] = RationalNumber[String](1)): Equation = {
    val rhs = rhsVars.map({
      case (symbol, (power, _, _)) => Expression.makePower(Variable(symbol), RationalNumber(power))
    }).reduce(_ * _)
    val expr = (constant * rhs) / Variable(lhs._1) - RationalNumber(1)
    def display(f: (String => VariableSpan)): DisplayMath = {
      DisplayMath(List(f(lhs._1), Span(" = "))) ++ DisplayMath.render((constant * rhs).mapVariables(f))
    }

    Equation(name, expr, display,
      rhsVars.mapValues(_._3) + (lhs._1 -> lhs._3),
      rhsVars.mapValues(_._2) + (lhs._1 -> lhs._2),
      tags.split(' ').toSet)
  }
}
