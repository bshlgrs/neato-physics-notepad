package workspace

import workspace.Workspace.VarId

import scala.util.{Failure, Try}


case class Equation(name: String,
                    expr: Expression[String],
                    dimensions: Map[String, Dimension],
                    varNames: Map[String, String]
                   ) {
  assert(expr.vars == dimensions.keys)
  assert(varNames.keys == dimensions.keys)

  def solve(varName: String, selfEqId: Int): Expression[VarId] = {
    expr.solve(varName).map((name) => (selfEqId, name))
  }

  def exprWithEquationId(id: Int): Expression[VarId] = expr.map((name) => (id, name))

  def vars: Set[String] = expr.vars

  def show(varNumbers: Map[String, Int]): String = {
    ???
  }
}

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
  } yield (equationId, varName)

  def addEquality(x: VarId, y: VarId): Workspace = this.copy(equalities = this.equalities.setEqual(x, y))

  def addExpression(varId: VarId): Workspace = varId match {
    case (eqId: Int, varName: String) => {
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

    val exprToSubIn = subbedSwapExpr.solve(varToRemoveId)

    // now sub that into the current expression
    val newExpr = exprs(exprVarId).sub(varToRemoveId, exprToSubIn)

    this.copy(exprs = exprs + (exprVarId -> newExpr))
  }

  def removeEquality(varId: VarId): Workspace = {
    this.copy(equalities = equalities.remove(varId))
  }

  def deleteEquation(id: Int): Workspace = {
    ???
  }

  def getNumber(varId: VarId): Option[PhysicalNumber] = numbers.values.find({ case (n, Some(varId2)) =>
    equalities.testEqual(varId, varId2)
  }).map(_._1)

  def attachNumber(numberId: Int, varId: VarId): Try[Workspace] = {
    val (eqIdx, varName) = varId
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
    val (eqIdx, varName) = varId
    equations(eqIdx).dimensions(varName)
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
        if otherEquation.expr.vars.exists((varName) => equalities.testEqual(otherEquationId -> varName, var1))
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

  def deleteExpression(id: (Int, String)): Workspace = this.copy(exprs = exprs - id)

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
    val varNumbers: Map[String, Int] = equation.vars.map((varName) => {
      varName -> allVarIds.filter({ case (idx2, name2) => idx2 < idx && varName == name2 })
               .groupBy(equalities.getSet)
               .size
    }).toMap
    equations(idx).show(varNumbers)
  }
}

case class InvalidActionException(comment: String) extends RuntimeException

object Workspace {
  val empty = Workspace(Map(), SetOfSets[VarId](Set()), Map(), Map())

  type VarId = (Int, String)
}
