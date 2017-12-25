package workspace

import scala.util.{Failure, Try}
import cas.Expression

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}


case class PhysicalNumber(value: Double, dimension: Dimension)

case class Workspace(equations: Map[Int, Equation],
                     equalities: SetOfSets[VarId],
                     exprs: Map[VarId, Expression[VarId]],
                     numbers: Map[Int, (PhysicalNumber, Option[VarId])]) {
  def lastEqId: Int = if (equations.isEmpty) -1 else equations.keys.max
  def lastNumberId: Int = if (numbers.isEmpty) -1 else numbers.keys.max
  def addEquation(equation: Equation): Workspace = this.copy(equations=this.equations + (this.lastEqId + 1 -> equation))
  def addNumber(physicalNumber: PhysicalNumber): Workspace =
    this.copy(numbers=this.numbers + (this.lastNumberId + 1 -> (physicalNumber, None)))

  def allVarIds: Set[VarId] = for {
    (equationId, equation) <- equations.toSet
    varName <- equation.vars
  } yield VarId(equationId, varName)

  def addEquality(x: VarId, y: VarId): Workspace = this.copy(equalities = this.equalities.setEqual(x, y))

  def addExpression(varId: VarId): Workspace = varId match {
    case VarId(eqId: Int, varName: String) => {
      val newExpr = equations(eqId).solve(varName, eqId)
      this.copy(exprs = exprs + (varId -> newExpr))
    }
  }

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
    println(solutions)
    assert(solutions.size == 1)
    val exprToSubIn = solutions.head

    // now sub that into the current expression
    val newExpr = exprs(exprVarId).substitute(varToRemoveId, exprToSubIn)
    this.copy(exprs = exprs + (exprVarId -> newExpr))
  }

  def removeEquality(varId: VarId): Workspace = {
    this.copy(equalities = equalities.remove(varId))
  }

  def deleteEquation(id: Int): Workspace = {
    ???
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

  // I think this isn't quite right
  def possibleRewritesForExpr(varId: VarId): Set[(VarId, Int)] = {
    val expr = exprs(varId)
    for {
      varId2 <- expr.vars
      (otherEquationId, otherEquation) <- equations
      // if otherEquation is actually a different equation than the one this variable comes from
      if otherEquationId != varId2.eqIdx
      // if otherEquation contains a related variable
      if otherEquation.expr.vars.exists((varName) => equalities.testEqual(VarId(otherEquationId, varName), varId2))
    } yield (varId2, otherEquationId)
  }

  def possibleActions: Set[FixedWorkspaceAction] = {
    {
      for {
        var1 <- allVarIds
        var2 <- allVarIds
        if getDimension(var1) == getDimension(var2)
        if !equalities.testEqual(var1, var2)
      } yield AddEqualityAction(var1, var2)
    } ++
      equalities.sets.flatten.map(RemoveEqualityAction) ++ {
      for {
        var1 <- allVarIds
        if !exprs.contains(var1)
      } yield AddExpressionAction(var1)
    } ++
      exprs.keys.map(DeleteExpressionAction) ++ {
      for {
        (var1, expr) <- exprs
        otherVar <- expr.vars
        (otherEquationId, otherEquation) <- equations
        // if otherEquation contains a related variable
        if otherEquation.expr.vars.exists((varName) => equalities.testEqual(VarId(otherEquationId, varName), var1))
      } yield RewriteExpressionAction(var1, otherVar, otherEquationId)
    } ++ {
      for {
        (numberId, (physicalNumber, attachment)) <- numbers
        var1 <- allVarIds
        if getDimension(var1) == physicalNumber.dimension
      } yield AttachNumberAction(numberId, var1)
    } ++ {
      for {
        (numberId, (physicalNumber, attachment)) <- numbers
        if attachment.isDefined
      } yield DetachNumberAction(numberId)
    } ++
    numbers.keys.map(DeleteNumberAction)
  }

  def deleteExpression(id: VarId): Workspace = this.copy(exprs = exprs - id)

  def handleAction(workspaceAction: WorkspaceAction): Try[Workspace] = workspaceAction match {
    case action: FixedWorkspaceAction => {
      if (possibleActions.contains(action)) {
        action match {
          case AddEqualityAction(varId1: VarId, varId2: VarId) => Try(addEquality(varId1, varId2))
          case RemoveEqualityAction(varId: VarId) => Try(removeEquality(varId))
          case AddExpressionAction(varId: VarId) => Try(addExpression(varId))
          case DeleteExpressionAction(varId: VarId) => Try(deleteExpression(varId))
          case RewriteExpressionAction(varId: VarId, varToRemoveId: VarId, equationToUseId: Int) => Try(
            this.rewriteExpression(varId, varToRemoveId, equationToUseId)
          )
          case AttachNumberAction(numberId: Int, varId: VarId) => attachNumber(numberId, varId)
          case DetachNumberAction(numberId: Int) => Try(detachNumber(numberId))
          case DeleteNumberAction(numberId: Int) => Try(deleteNumber(numberId))
        }
      } else {
        Failure(InvalidActionException("your action wasn't one of the allowed ones"))
      }
    }
    case AddEquationAction(equation) => Try(this.addEquation(equation))
    case AddNumberAction(number) => Try(this.addNumber(number))
  }

  def showEquation(idx: Int): String = {
    val equation = equations(idx)
    val varSubscripts: Map[String, Int] = equation.vars.map((varName) => {
      varName -> getVarSubscript(VarId(idx, varName))
    }).toMap.collect({case (k, Some(v)) => k -> v})
    LatexString.showEquation(equations(idx), varSubscripts)
  }

  def showExpression(exprVarId: VarId): String = {
    val expression = exprs(exprVarId)

    val varSubscripts: Map[VarId, Int] = (expression.vars + exprVarId).map((varId) => {
      varId -> getVarSubscript(varId)
    }).toMap.collect({case (k, Some(v)) => k -> v})

    LatexString.showExpression(exprVarId, expression, varSubscripts)
  }

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
