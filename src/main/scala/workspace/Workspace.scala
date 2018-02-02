package workspace

import scala.util.{Failure, Success, Try}
import cas._
import workspace.dimensions._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel, ScalaJSDefined}

@JSExportAll
@JSExportTopLevel("Gem.Workspace")
case class Workspace(equations: MapWithIds[Equation],
                     equalities: SetOfSets[VarId],
                     expressions: Map[VarId, Expression[VarId]],
                     numbers: MapWithIds[(PhysicalNumber, Option[VarId])],
                     diagrams: MapWithIds[TriangleDiagram]
                    ) {
  def toJsObject: js.Object = js.Dynamic.literal(
    "className" -> "Workspace",
    "equations" -> js.Array(equations.map.toSeq.map({ case (x: Int, y: Equation) => js.Tuple2(x, y.toJsObject) }) :_*),
    "equalities" -> js.Array(equalities.sets.toList.map(set => js.Array(set.toList.map(_.toJsObject) :_*)) :_*),
    "expressions" -> js.Array(expressions.toSeq.map({ case (x: VarId, y: Expression[VarId]) => js.Tuple2(x.toJsObject, y.toJsObject)}) :_*),
    "numbers" -> js.Array(numbers.map.toSeq.map(
      { case (id: Int, (n: PhysicalNumber, v: Option[VarId])) => js.Tuple3(id, n.toJsObject, v.map(_.toJsObject).orNull)}) :_*)
  )

  def equationIds: js.Array[Int] = arr(equations.keySet)
  def getEquation(id: Int): Equation = equations.get(id).orNull
  def expressionIds: js.Array[VarId] = arr(expressions.keySet)
  def numberIds: js.Array[Int] = arr(numbers.keySet)
  def getNumber(numberId: Int): PhysicalNumber = numbers.getOrElse(numberId, null)._1
  def getVarIdOfNumber(numberId: Int): VarId = numbers.get(numberId).flatMap(_._2).orNull

  def addableExpressionIds: js.Array[VarId] = {
    arr(allVarIds.filter((varId) => equalities.getSet(varId).forall((varId2) => !expressions.contains(varId2))))
  }

  // TODO, kill these
  def nextEqId: Int = equations.nextId
  def nextNumberId: Int = numbers.nextId

  def addEquation(equation: Equation): Workspace = this.copy(equations=this.equations.addWithNextId(equation))

  def addNumber(physicalNumber: PhysicalNumber): Workspace = this.copy(numbers=this.numbers.addWithNextId((physicalNumber, None)))
  def addAndAttachNumber(varId: VarId, physicalNumber: PhysicalNumber): Workspace = this.addNumber(physicalNumber).attachNumber(this.nextNumberId, varId).get

  def allVarIds: Set[VarId] = (for {
    (equationId, equation) <- equations.toSet
    varName <- equation.vars
  } yield EquationVarId(equationId, varName)) ++ (for {
    diagramId <- diagrams.keys
    varName <- diagrams(diagramId).diagramVarNames
  } yield {
    DiagramVarId(diagramId, varName)
  })

  def allVarIdsJs: js.Array[VarId] = arr(allVarIds)
  def varIdStringToVarId(str: String): VarId = allVarIds.find(_.toString == str).orNull

  def addEquality(x: VarId, y: VarId): Workspace = this.copy(equalities = this.equalities.setEqual(x, y))

  def addExpression(varId: VarId): Workspace = varId match {
    case EquationVarId(eqId: Int, varName: String) => {
      val newExpr = equations(eqId).solve(varName, eqId)
      this.copy(expressions = expressions + (varId -> newExpr))
    }
  }

  lazy val allVarsGroupedByEquality: js.Array[js.Array[VarId]] = {
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

  def varIsEqualToAnything(varId: VarId): Boolean = equalities.getSet(varId).size > 1

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
    // TODO: generalize to work for diagram VarIDs
    val EquationVarId(eqIdx, varName) = varId
    for {
      (number, currentAttachment) <- Try(numbers.get(numberId).get)
      eq: Equation <- Try(this.equations.get(eqIdx).get)
      mbVariableDimension: Option[SiDimension] <- Try(getDimensionDirectly(varId))
      _ <- mbVariableDimension match {
        case Some(variableDimension) => Try(assert(variableDimension == number.siDimension), "var dimension does not match")
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

  def getDimensionDirectly(varId: VarId): Option[SiDimension] = varId match {
    case EquationVarId(eqIdx, varName) => equations(eqIdx).staticDimensions.get(varName).orElse(getNumber(varId).map(_.siDimension))
    // TODO: Also you can check it by looking for the dimensions of variables it's equated to, or its number.
    case DiagramVarId(diagramIdx, varName) => None
      // TODO: implement this
  }

  def getDimension(varId: VarId): Option[SiDimension] = allDimensions.get(varId).flatten

  lazy val allDimensions: Map[VarId, Option[SiDimension]] = allVarIds.map(varId => varId -> getDimensionCalc(varId)).toMap

  def getDimensionCalc(varId: VarId): Option[SiDimension] = {
    val calculatedTypes: Set[DimensionInference] = equalities.getSet(varId).flatMap({
      case varId@EquationVarId(eqIdx, varName) => {
        val solutions = equations(eqIdx).solutions(varName, eqIdx)
        val types = solutions.map(_.calculateDimension((x) => DimensionInference.fromTopOption(getDimensionDirectly(x)))) ++
           Set(DimensionInference.fromTopOption(getDimensionDirectly(varId)))
        types
      }
      case DiagramVarId(diagramIdx, varName) => {
        // todo: actual inference here
        Set[DimensionInference]()
      }
    })
    calculatedTypes.reduceOption(_ combineWithEquals _).getOrElse(TopDimensionInference) match {
      case BottomDimensionInference => Util.err(s"Types were inconsistent for $varId: $calculatedTypes")
      case TopDimensionInference => None
      case ConcreteDimensionInference(dim) => Some(dim)
    }
  }

  def getDimensionJs(varId: VarId): SiDimension = getDimension(varId).orNull

  def possibleRewritesForExpr(exprVarId: VarId): Set[(VarId, Int)] = {
    val expr = expressions(exprVarId)
    for {
      varToRemoveId <- expr.vars
      equationIdToUse <- equations.keys

      // if equationToUse contains a related variable
      if checkRewriteAttemptIsValid(exprVarId, varToRemoveId, equationIdToUse)
    } yield (varToRemoveId, equationIdToUse)
  }

  def checkRewriteAttemptIsValid(exprVarId: VarId, varToRemoveId: VarId, equationIdToUse: Int): Boolean = {
    val equation = equations(equationIdToUse)
    val varIds = equation.vars.map(name => EquationVarId(equationIdToUse, name))
    varIds.exists(varId => equalities.testEqual(varId, varToRemoveId))
  }

  def possibleRewritesForExprJs(varId: VarId): js.Array[js.Any] = arr(possibleRewritesForExpr(varId).map(x => js.Array(x._1, x._2)))

  def deleteExpression(id: VarId): Workspace = this.copy(expressions = expressions - id)

  def getEquationBuckTex(idx: Int): BuckTex = {
    val equation = equations(idx)
    val varSubscripts: Map[String, Int] = equation.vars.map((varName) => {
      varName -> getVarSubscript(EquationVarId(idx, varName))
    }).toMap.collect({case (k, Some(v)) => k -> v})
    CompileToBuckTex.showEquation(equation, idx, varSubscripts)
  }

  def getNumberForExpression(exprVarId: VarId): Option[Double] = recursivelyEvaluatedNumbers.get(exprVarId)
  def getNumberForExpressionJs(exprVarId: VarId): Any = recursivelyEvaluatedNumbers.get(exprVarId).orNull

  lazy val recursivelyEvaluatedNumbers: Map[VarId, Double] = {
    def evalNumbers(knownNumbers: Map[Set[VarId], Double]): Map[Set[VarId], Double] = {
      val newNumbers: Map[Set[VarId], Double] = knownNumbers ++ expressions.map({ case (v: VarId, e: Expression[VarId]) => {
        v -> e.mapVariablesToExpressions(varId2 => knownNumbers.get(equalities.getSet(varId2)) match {
          case None => Variable(varId2)
          case Some(value) => RealNumber[VarId](value)
      })}}).mapValues(_.evaluate).collect({ case (k, Some(v)) => equalities.getSet(k) -> v })
      if (newNumbers != knownNumbers) {
        evalNumbers(newNumbers)
      } else newNumbers
    }

    val setValue = evalNumbers(numbers.map.collect({case (_: Int, (n: PhysicalNumber, Some(varId))) =>
      equalities.getSet(varId) -> n.value
    }))

    Util.traceShow(setValue.flatMap({ case (setVarId, value) => setVarId.map(_ -> value) }))
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
        varId.sourceIsGreaterThan(varId2) && // We are counting variables from earlier equations
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
    case (Some(x), Some(y)) => x == y
    case _ => true
  }

  def consistentUnitsWithDimension(varId1: VarId, dimension: SiDimension): Boolean = getDimensionDirectly(varId1) match {
    case Some(var1Dimension) => var1Dimension == dimension
    case _ => true
  }

  def changeDimension(id: Integer, dimension: Dimension): Workspace = {
    val (oldNumber, mbAttachment) = numbers(id)
    val newNumber = oldNumber.changeDimension(dimension)
    val newNumbers = numbers.set(id, (newNumber, mbAttachment))
    this.copy(numbers = newNumbers)
  }

  def addDiagram: Workspace = this.copy(diagrams = this.diagrams.addWithNextId(TriangleDiagram(Map())))
  def setDiagramVar(diagramId: Int, varName: String, newSetting: Option[VarId]): Workspace = {
    val d = diagrams(diagramId)

    this.copy(diagrams = this.diagrams.set(diagramId, d.set(varName, newSetting)))
  }

  def setDiagramVarToVarId(diagramId: Int, varName: String, newSetting: VarId): Workspace =
    setDiagramVar(diagramId, varName, Some(newSetting))
  def setDiagramVarToBlank(diagramId: Int, varName: String): Workspace =
    setDiagramVar(diagramId, varName, None)

  def deleteDiagram(diagramId: Int): Workspace = this.copy(diagrams = this.diagrams.delete(diagramId))

  def addEquationFromDiagram(diagramId: Int, equation: CustomEquation): Workspace = {
    val equalitiesToAdd = this.diagrams(diagramId).equalitiesToAddWithEquation(equation, this.equations.nextId)

    equalitiesToAdd.foldLeft(this.addEquation(equation)) ({
      case (ws, (varId1, varId2)) => ws.addEquality(varId1, varId2)
    })
  }

  def diagramVarBuckTexJs(diagramId: Int, diagramVarName: String): BuckTex = {
    this.diagrams(diagramId).vars.get(diagramVarName) match {
      case None => getVariableBuckTex(DiagramVarId(diagramId, diagramVarName))
      case Some(varId) => getVariableBuckTex(varId)
    }
  }
}

@JSExportTopLevel("Gem.WorkspaceOps")
@JSExportAll
object Workspace {
  def empty: Workspace = Workspace(MapWithIds.empty, SetOfSets[VarId](Set()), Map(), MapWithIds.empty, MapWithIds.empty)
}


trait WorkspaceJs extends js.Object {
  val className: String
  val equations: js.Array[js.Tuple2[Int, EquationJs]]
  val equalities: js.Array[js.Array[VarIdJs]]
  val expressions: js.Array[js.Tuple2[VarIdJs, ExpressionJs]]
  val numbers: js.Array[js.Tuple3[Int, PhysicalNumberJs, js.UndefOr[VarIdJs]]]
}

@JSExportTopLevel("Gem.WorkspaceJs")
object WorkspaceJs {
  @JSExport
  def parse(ws: WorkspaceJs, library: EquationLibrary): Workspace = {
    import Util.annotateFailWithMessage

    val equations = annotateFailWithMessage("equations",
      MapWithIds.fromMap(
        ws.equations.map({ case js.Tuple2(id: Int, eq: EquationJs) => id -> EquationJs.parse(eq, library)}).toMap
      ))
    val equalities = annotateFailWithMessage("equalities", SetOfSets(ws.equalities.map(_.map(VarIdJs.parse).toSet).toSet))
    val expressions = annotateFailWithMessage("expressions", ws.expressions.map({
      case js.Tuple2(varIdJs: VarIdJs, expr: ExpressionJs) => VarIdJs.parse(varIdJs) -> ExpressionJs.parseToVarIdExpr(expr)
    }).toMap)
    val numbers = annotateFailWithMessage("numbers", MapWithIds(ws.numbers.map({ case js.Tuple3(id: Int, num: PhysicalNumberJs, mbVarId: js.UndefOr[VarIdJs]) =>
      println(scalajs.js.JSON.stringify(num))
      id -> (annotateFailWithMessage("parse1", PhysicalNumberJs.parse(num)) -> annotateFailWithMessage("parse2", mbVarId.toOption.map(VarIdJs.parse)))
    }).toMap))

    // todo: save triangles
    Workspace(equations, equalities, expressions, numbers, MapWithIds.empty)
  }
}
