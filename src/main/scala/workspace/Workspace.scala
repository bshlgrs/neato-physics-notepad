package workspace

import scala.util.{Failure, Try}
import cas.Expression
import wrapper.ToJS

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}




@JSExportAll
@JSExportTopLevel("Gem.Workspace")
case class Workspace(equations: Map[Int, Equation] = Map(),
                     equalities: SetOfSets[VarId] = SetOfSets(Set()),
                     expressions: Map[VarId, Expression[VarId]] = Map(),
                     numbers: Map[Int, (PhysicalNumber, Option[VarId])] = Map()) {

  def equationIds: js.Array[Int] = arr(equations.keys.toSet)
  def expressionIds: js.Array[VarId] = arr(expressions.keys.toSet)
  def addableExpressionIds: js.Array[VarId] = {
    arr(allVarIds.filter((varId) => equalities.getSet(varId).forall((varId2) => !expressions.contains(varId2))))
  }

  def lastEqId: Int = if (equations.isEmpty) -1 else equations.keys.max
  def lastNumberId: Int = if (numbers.isEmpty) -1 else numbers.keys.max

  def addEquation(equation: Equation): Workspace = this.copy(equations=this.equations + (this.lastEqId + 1 -> equation))

  def addNumber(physicalNumber: PhysicalNumber): Workspace =
    this.copy(numbers=this.numbers + (this.lastNumberId + 1 -> (physicalNumber, None)))

  def allVarIds: Set[VarId] = for {
    (equationId, equation) <- equations.toSet
    varName <- equation.vars
  } yield VarId(equationId, varName)

  def allVarIdsJs: js.Array[VarId] = arr(allVarIds)
  def varIdStringToVarId(str: String): VarId = allVarIds.find(_.toString() == str).get

  def addEquality(x: VarId, y: VarId): Workspace = this.copy(equalities = this.equalities.setEqual(x, y))

  def addExpression(varId: VarId): Workspace = varId match {
    case VarId(eqId: Int, varName: String) => {
      val newExpr = equations(eqId).solve(varName, eqId)
      this.copy(expressions = expressions + (varId -> newExpr))
    }
  }

  def equalityListOfLists: js.Array[js.Array[VarId]] = arr(equalities.sets.map(arr))

  def rewriteExpression(exprVarId: VarId, varToRemoveId: VarId, equationIdToUse: Int): Workspace = {
    // This method means "Solve equation number equationIdToUse for varToRemove and then substitute the result into the
    // expression for exprVar."

    // Eg maybe you have z = x^2 * y, and you're going to change that by replacing x with an expression for x you got
    // from a different equation.

    // let's look up the equation we're going to solve to use:
    val swapEquationExpr: Expression[VarId] = equations(equationIdToUse).exprWithEquationId(equationIdToUse)

    // now we replace every variable in that expression which is equivalent to our swapped variable with the swapped
    // variable.
    val subbedSwapExpr = swapEquationExpr.simplifyWithEquivalenceClasses(equalities)

    val solutions = subbedSwapExpr.solve(equalities.getSet(varToRemoveId).head)

    assert(solutions.size == 1)
    val exprToSubIn = solutions.head

    // now sub that into the current expression
    val newExpr = expressions(exprVarId).substitute(varToRemoveId, exprToSubIn)
    this.copy(expressions = expressions + (exprVarId -> newExpr))
  }

  def removeEquality(varId: VarId): Workspace = {
    this.copy(equalities = equalities.remove(varId))
  }

  def deleteEquation(id: Int): Workspace = {
    this.copy(equations = equations - id)
  }

  def getNumber(varId: VarId): Option[PhysicalNumber] = numbers.values.find({
    case (n, Some(varId2)) => equalities.testEqual(varId, varId2)
    case _ => false
  }).map(_._1)

  def attachNumber(numberId: Int, varId: VarId): Try[Workspace] = {
    val VarId(eqIdx, varName) = varId
    for {
      (number, currentAttachment) <- Try(numbers.get(numberId).get)
      eq: Equation <- Try(this.equations.get(eqIdx).get)
      variableDimension: Dimension <- Try(eq.dimensions.get(varName).get)
      _ <- Try({
        assert(variableDimension == number.dimension, "var dimension does not match")
      })
    } yield this.copy(numbers = numbers + (numberId -> (number -> Some(varId))))
  }

  def detachNumber(numberId: Int): Workspace = {
    val physicalNumber = numbers(numberId)._1
    this.copy(numbers = numbers + (numberId -> (physicalNumber -> None)))
  }

  def deleteNumber(numberId: Int): Workspace = {
    this.copy(numbers = numbers - numberId)
  }

  def getDimension(varId: VarId): Dimension = {
    equations(varId.eqIdx).dimensions(varId.varName)
  }

  def possibleRewritesForExpr(varId: VarId): Set[(VarId, Int)] = {
    val expr = expressions(varId)
    for {
      varId2 <- expr.vars
      (otherEquationId, otherEquation) <- equations
      // if otherEquation is actually a different equation than the one this variable comes from
      if otherEquationId != varId2.eqIdx
      // if otherEquation contains a related variable
      if otherEquation.expr.vars.exists((varName) => equalities.testEqual(VarId(otherEquationId, varName), varId2))
    } yield (varId2, otherEquationId)
  }

  def possibleRewritesForExprJs(varId: VarId): js.Array[js.Any] = arr(possibleRewritesForExpr(varId).map(x => js.Array(x._1, x._2)))

  def deleteExpression(id: VarId): Workspace = this.copy(expressions = expressions - id)

  def showEquation(idx: Int): String = {
    ???
    val equation = equations(idx)
    val varSubscripts: Map[String, Int] = equation.vars.map((varName) => {
      varName -> getVarSubscript(VarId(idx, varName))
    }).toMap.collect({case (k, Some(v)) => k -> v})
//    StringDisplay.showEquation(equations(idx), varSubscripts)
    // Currently broken-- I broke showEquation because I didn't need it
    ???
  }

  def getEquationDisplay(idx: Int): DisplayMath = {
    val equation = equations(idx)
    val varSubscripts: Map[String, Int] = equation.vars.map((varName) => {
      varName -> getVarSubscript(VarId(idx, varName))
    }).toMap.collect({case (k, Some(v)) => k -> v})
    DisplayMath.showEquation(equation, idx, varSubscripts)
  }

  def showExpression(exprVarId: VarId): String = {
    val expression = expressions(exprVarId)

    val varSubscripts: Map[VarId, Int] = (expression.vars + exprVarId).map((varId) => {
      varId -> getVarSubscript(varId)
    }).toMap.collect({case (k, Some(v)) => k -> v})

    StringDisplay.showExpression(exprVarId, expression, varSubscripts)
  }

  def getExpressionDisplay(exprVarId: VarId): DisplayMath = {
    val expression = expressions(exprVarId)
    val varSubscripts: Map[VarId, Int] = (expression.vars + exprVarId).map((varId) => {
      varId -> getVarSubscript(varId)
    }).toMap.collect({case (k, Some(v)) => k -> v})

    DisplayMath.showExpression(exprVarId, expression, varSubscripts)
  }

  def showVar(varId: VarId): String = StringDisplay.showVar(varId.varName, getVarSubscript(varId))

  def getVarSubscript(varId: VarId): Option[Int] = {
    // If, of the variables that aren't equal to you, none of them share your name, you don't need a subscript.
    if (allVarIds.filter(!equalities.testEqual(_, varId)).count(_.varName == varId.varName) == 0)
      None
    else {
      val relevantVarIds = allVarIds.filter(varId2 =>
        varId.eqIdx > varId2.eqIdx && // We are counting variables from earlier equations
          varId.varName == varId2.varName && // that have the same name
          !equalities.testEqual(varId, varId2)) // and that aren't equal
      val groups = relevantVarIds.groupBy(equalities.getSet)

      Some(groups.size + 1)
    }
  }

  def addableEqualities: Set[(VarId, VarId)] = {
    for {
      var1 <- allVarIds
      var2 <- allVarIds
      if getDimension(var1) == getDimension(var2)
      if !equalities.testEqual(var1, var2)
      if var1.toString > var2.toString
    } yield (var1, var2)
  }

  def addableEqualitiesJs: js.Array[js.Array[VarId]] = {
    arr(addableEqualities.toList.sortBy(_.toString).map((x) => js.Array(x._1, x._2)))
  }

  private def arr[A](x: Iterable[A]): js.Array[A] = js.Array(x.toSeq : _*)

}

case class InvalidActionException(comment: String) extends RuntimeException

@JSExportTopLevel("Gem.VarId")
@JSExportAll
case class VarId(eqIdx: Int, varName: String)

@JSExportTopLevel("Gem.WorkspaceOps")
@JSExportAll
object Workspace {
  def empty = Workspace(Map(), SetOfSets[VarId](Set()), Map(), Map())
}
