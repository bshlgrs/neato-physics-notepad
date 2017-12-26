package workspace

import cas.Expression

case class Equation(name: String,
                    expr: Expression[String],
                    display: (String => VariableSpan) => DisplayMath,
                    dimensions: Map[String, Dimension],
                    varNames: Map[String, String]
                   ) {
  assert(expr.vars == dimensions.keys)
  assert(varNames.keys == dimensions.keys)

  def solve(varName: String, selfEqId: Int): Expression[VarId] = {
    // TODO: check on the `head` here
    expr.solve(varName).head.mapVariables((name) => VarId(selfEqId, name))
  }

  def exprWithEquationId(id: Int): Expression[VarId] = expr.mapVariables((name) => VarId(id, name))

  def vars: Set[String] = expr.vars
}
