package workspace

import workspace.Workspace.VarId


case class Equation(expr: Expression[String]) {
  def solve(varName: String, selfEqId: Int): Expression[VarId] = {
    expr.solve(varName).map((name) => (selfEqId, name))
  }

  def exprWithEquationId(id: Int): Expression[VarId] = expr.map((name) => (id, name))
}

case class Workspace(equations: Map[Int, Equation], equalities: SetOfSets[VarId], exprs: Map[VarId, Expression[VarId]]) {
  def lastEqId: Int = (equations.keys ++ List(-1)).max
  def addEquation(equation: Equation) = this.copy(equations=this.equations + (this.lastEqId + 1 -> equation))
  def setEquality(x: VarId, y: VarId): Workspace = this.copy(equalities = this.equalities.setEqual(x, y))

  def solve(varId: VarId): Workspace = varId match {
    case (eqId: Int, varName: String) => {
      val newExpr = equations(eqId).solve(varName, eqId)
      this.copy(exprs = exprs + (varId -> newExpr))
    }
  }
  def subExpr(exprVarId: VarId, swappedOutVarId: VarId, equationIdToUse: Int): Workspace = {
    // This method means "Modify the expression for exprVarId by solving equation number equationIdToUse for
    // swappedOutVarId and then substituting that into the expression."

    // Eg maybe you have z = x^2 * y, and you're going to change that by replacing x with an expression for x you got
    // from a different equation.


    // now let's look up the equation we're going to solve to use:
    val swapEquationExpr: Expression[VarId] = equations(equationIdToUse).exprWithEquationId(equationIdToUse)

    // now we replace every variable in that expression which is equivalent to our swapped variable with the swapped
    // variable.
    val subbedSwapExpr = swapEquationExpr.simplifyWithEquivalenceClasses(equalities)

    val exprToSubIn = subbedSwapExpr.solve(swappedOutVarId)

    // now sub that into the current expression
    val newExpr = exprs(exprVarId).sub(swappedOutVarId, exprToSubIn)

    this.copy(exprs = exprs + (exprVarId -> newExpr))
  }

  def undoEquality(varId: VarId): Workspace = {
    this.copy(equalities = equalities.remove(varId))
  }
}


object Workspace {
  val empty = Workspace(Map(), SetOfSets[VarId](Set()), Map())

  type VarId = (Int, String)

  def main(args: Array[String]): Unit = {
    val ws = Workspace.empty
      .addEquation(EquationLibrary.getByEqId("ke_def"))
      .addEquation(EquationLibrary.getByEqId("pe_def"))
      .setEquality((0, "m"), (1, "m"))
      .setEquality((0, "KE"), (1, "PE"))
      .solve((0, "v"))

    val ws2 = ws.subExpr((0, "v"), (0, "KE"), 1)
    println(ws.exprs)
    println(ws2.exprs)
  }
}
