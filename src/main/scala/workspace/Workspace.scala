package workspace

import scala.util.{Failure, Success, Try}
import cas.{Expression, RationalNumber, RealNumber}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportAll
@JSExportTopLevel("Gem.Workspace")
case class Workspace(equations: MapWithIds[Equation] = MapWithIds.empty[Equation],
                     equalities: SetOfSets[VarId] = SetOfSets(Set()),
                     expressions: Map[VarId, Expression[VarId]] = Map(),
                     numbers: MapWithIds[(PhysicalNumber, Option[VarId])] = MapWithIds.empty) {

  def equationIds: js.Array[Int] = arr(equations.keySet)
  def getEquation(id: Int): Equation = equations.get(id).orNull
  def expressionIds: js.Array[VarId] = arr(expressions.keySet)
  def numberIds: js.Array[Int] = arr(numbers.keySet)
  def getNumber(numberId: Int): PhysicalNumber = numbers.getOrElse(numberId, null)._1
  def getVarIdOfNumber(numberId: Int): VarId = numbers.get(numberId).flatMap(_._2).orNull

  def addableExpressionIds: js.Array[VarId] = {
    arr(allVarIds.filter((varId) => equalities.getSet(varId).forall((varId2) => !expressions.contains(varId2))))
  }

  def nextEqId: Int = equations.nextId
  def nextNumberId: Int = numbers.nextId

  def addEquation(equation: Equation): Workspace = this.copy(equations=this.equations.addWithNextId(equation))

  def addNumber(physicalNumber: PhysicalNumber): Workspace = this.copy(numbers=this.numbers.addWithNextId((physicalNumber, None)))

  def allVarIds: Set[VarId] = for {
    (equationId, equation) <- equations.toSet
    varName <- equation.vars
  } yield VarId(equationId, varName)

  def allVarIdsJs: js.Array[VarId] = arr(allVarIds)
  def varIdStringToVarId(str: String): VarId = allVarIds.find(_.toString == str).get

  def addEquality(x: VarId, y: VarId): Workspace = this.copy(equalities = this.equalities.setEqual(x, y))

  def addExpression(varId: VarId): Workspace = varId match {
    case VarId(eqId: Int, varName: String) => {
      val newExpr = equations(eqId).solve(varName, eqId)
      this.copy(expressions = expressions + (varId -> newExpr))
    }
  }

  def allVarsGroupedByEquality: js.Array[js.Array[VarId]] = {
    arr(allVarIds.groupBy(varId => equalities.getSet(varId)).values.map(arr))
  }

  def rewriteExpression(exprVarId: VarId, varToRemoveId: VarId, equationIdToUse: Int): Workspace = {
    // This method means "Solve equation number equationIdToUse for varToRemove and then substitute the result into the
    // expression for exprVar."

    // Eg maybe you have z = x^2 * y, and you're going to change that by replacing x with an expression for x you got
    // from a different equation.

    // let's look up the equation we're going to solve to use:
    val two = RationalNumber(2)
    val swapEquationExpr: Expression[VarId] = equations(equationIdToUse).exprWithEquationId(equationIdToUse)
//    assert(swapEquationExpr.vars.contains(varToRemoveId))

    // now we replace every variable in that expression which is equivalent to our swapped variable with the swapped
    // variable.
    val subbedSwapExpr = swapEquationExpr.simplifyWithEquivalenceClasses(equalities)

    val normalizedVarToRemoveId = equalities.getSet(varToRemoveId).minBy(_.toString)
    val solutions = subbedSwapExpr.solve(normalizedVarToRemoveId)

    assert(solutions.size == 1, solutions)
    val exprToSubIn = solutions.head

    // now sub that into the current expression
    val normalizedCurrentExpr = expressions(exprVarId).simplifyWithEquivalenceClasses(equalities)
    val newExpr = normalizedCurrentExpr.substitute(normalizedVarToRemoveId, exprToSubIn)
    this.copy(expressions = expressions + (exprVarId -> newExpr))
  }

  def removeEquality(varId: VarId): Workspace = {
    this.copy(equalities = equalities.remove(varId))
  }

  def deleteEquation(id: Int): Workspace = {
    this.copy(equations = equations.delete(id))
  }

  def getNumber(varId: VarId): Option[PhysicalNumber] = numbers.values.find({
    case (n, Some(varId2)) => equalities.testEqual(varId, varId2)
    case _ => false
  }).map(_._1)

  def getNumberIdOfVar(varId: VarId): Any = numbers.find({
    case (_, (n, Some(varId2))) => equalities.testEqual(varId, varId2)
    case _ => false
  }).map(_._1).orNull

  def attachNumber(numberId: Int, varId: VarId): Try[Workspace] = {
    // I am kind of suspicious of this function
    val VarId(eqIdx, varName) = varId
    for {
      (number, currentAttachment) <- Try(numbers.get(numberId).get)
      eq: Equation <- Try(this.equations.get(eqIdx).get)
      mbVariableDimension: Option[SiDimension] <- Try(getDimensionDirectly(VarId(eqIdx, varName)))
      _ <- mbVariableDimension match {
        case Some(variableDimension) => Try(assert(variableDimension.equalUnits(number.siDimension), "var dimension does not match"))
        case _ => Success()
      }
    } yield {
      val detachedWorkspace: Workspace = getNumberIdOfVar(varId) match {
        case numId: Int => this.detachNumber(numId); case _ => this
      }

      detachedWorkspace.copy(numbers = detachedWorkspace.numbers.set(numberId, number -> Some(varId)))
    }
  }

  def attachNumberJs(numberId: Int, varId: VarId): Workspace = attachNumber(numberId, varId).toOption.orNull

  def detachNumber(numberId: Int): Workspace = {
    val physicalNumber = numbers(numberId)._1
    this.copy(numbers = numbers.set(numberId, physicalNumber -> None))
  }

  def deleteNumber(numberId: Int): Workspace = {
    this.copy(numbers = numbers.delete(numberId))
  }

  def getDimensionDirectly(varId: VarId): Option[SiDimension] = {
    equations(varId.eqIdx).staticDimensions.get(varId.varName).orElse(getNumber(varId).map(_.siDimension))
    // TODO: Also you can check it by looking for the dimensions of variables it's equated to, or its number.
  }

  def getDimension(varId: VarId): Option[SiDimension] = {
    equalities.getSet(varId).flatMap({ case VarId(eqIdx, varName) => {
      equations(eqIdx).solutions(varName, eqIdx).map(_.calculateDimension((x) => DimensionInference.fromTopOption(getDimensionDirectly(x))))
    }}).reduce(_ combineWithEquals _).asTopOption
  }

  def getDimensionJs(varId: VarId): SiDimension = getDimension(varId).orNull

  def possibleRewritesForExpr(exprVarId: VarId): Set[(VarId, Int)] = {
    // todo: prevent allowing you to make tautologies like "v = v"
    val expr = expressions(exprVarId)
    for {
      varToRemoveId <- expr.vars
      equationIdToUse <- equations.keys
      // if equationToUse is actually a different equation than the one this variable comes from
//      if equationIdToUse != exprVarId.eqIdx

      // if equationToUse contains a related variable
      if checkRewriteAttemptIsValid(exprVarId, varToRemoveId, equationIdToUse)
    } yield (varToRemoveId, equationIdToUse)
  }

  def checkRewriteAttemptIsValid(exprVarId: VarId, varToRemoveId: VarId, equationIdToUse: Int): Boolean = {
    val equation = equations(equationIdToUse)
    val varIds = equation.vars.map(name => VarId(equationIdToUse, name))
    varIds.exists(varId => equalities.testEqual(varId, varToRemoveId))
  }

  def possibleRewritesForExprJs(varId: VarId): js.Array[js.Any] = arr(possibleRewritesForExpr(varId).map(x => js.Array(x._1, x._2)))

  def deleteExpression(id: VarId): Workspace = this.copy(expressions = expressions - id)

  def getEquationBuckTex(idx: Int): BuckTex = {
    val equation = equations(idx)
    val varSubscripts: Map[String, Int] = equation.vars.map((varName) => {
      varName -> getVarSubscript(VarId(idx, varName))
    }).toMap.collect({case (k, Some(v)) => k -> v})
    CompileToBuckTex.showEquation(equation, idx, varSubscripts)
  }

  def getNumberForExpression(exprVarId: VarId): Option[Double] = {
    val expr = expressions(exprVarId)
    val maximallyNumericExpression = expr.vars.foldLeft(expr)((reducedExpr, varId) => this.getNumber(varId) match {
      case None => reducedExpr
      case Some(num) => reducedExpr.substitute(varId, RealNumber[VarId](num.value))
    })
    maximallyNumericExpression.evaluate
  }

  def getNumberForExpressionJs(exprVarId: VarId): Any = {
    getNumberForExpression(exprVarId).orNull
  }

  def getExpressionBuckTex(exprVarId: VarId): BuckTex = {
    val expression = expressions(exprVarId)
    val varSubscripts: Map[VarId, Int] = (expression.vars + exprVarId).map((varId) => {
      varId -> getVarSubscript(varId)
    }).toMap.collect({case (k, Some(v)) => k -> v})

    CompileToBuckTex.showExpression(exprVarId, expression, varSubscripts,
      this.getNumberForExpression(exprVarId).map((number) =>
        PhysicalNumber(number, getDimension(exprVarId).getOrElse(SiDimension.Dimensionless), None)))
  }

  def getVariableBuckTex(varId: VarId): BuckTex = {
    val subscripts = Map(varId -> getVarSubscript(varId)).filter(_._2.isDefined).mapValues(_.get)
    CompileToBuckTex.showVariable(varId, subscripts)
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

  def addableEqualities: Set[(VarId, VarId)] = {
    for {
      var1 <- allVarIds
      var2 <- allVarIds
      if getDimensionDirectly(var1) == getDimensionDirectly(var2)
      if !equalities.testEqual(var1, var2)
      if var1.toString > var2.toString
    } yield (var1, var2)
  }

  def addableEqualitiesJs: js.Array[js.Array[VarId]] = {
    arr(addableEqualities.toList.sortBy(_.toString).map((x) => js.Array(x._1, x._2)))
  }

  private def arr[A](x: Iterable[A]): js.Array[A] = js.Array(x.toSeq : _*)

  def consistentUnits(varId1: VarId, varId2: VarId): Boolean = (getDimensionDirectly(varId1), getDimensionDirectly(varId2)) match {
    case (Some(x), Some(y)) => x.equalUnits(y)
    case _ => true
  }

  def consistentUnitsWithDimension(varId1: VarId, dimension: SiDimension): Boolean = getDimensionDirectly(varId1) match {
    case Some(var1Dimension) => var1Dimension.equalUnits(dimension)
    case _ => true
  }
}

case class InvalidActionException(comment: String) extends RuntimeException

@JSExportTopLevel("Gem.VarId")
@JSExportAll
case class VarId(eqIdx: Int, varName: String)

@JSExportTopLevel("Gem.WorkspaceOps")
@JSExportAll
object Workspace {
  def empty = Workspace(MapWithIds.empty, SetOfSets[VarId](Set()), Map(), MapWithIds.empty)
}

