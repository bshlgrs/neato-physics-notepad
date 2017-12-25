package wrapper

import workspace._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportTopLevel("Gem.Workspace")
@JSExportAll
case class JsWorkspace(ws: Workspace = Workspace.empty) {
  def equations = js.Dictionary(ws.equations.toList.map({ case (x, y) => x.toString -> y }) : _*)
  def equationList = js.Array(ws.equations.toList.map({ case (x, y) => x.toString -> y }) : _*)

  def addEquationFromLibrary(id: String): JsWorkspace = this.copy(ws = ws.addEquation(EquationLibrary.getByEqId(id)))

  def equationLatex(idx: Int): String = ws.showEquation(idx)

  def addableEqualities: js.Array[js.Array[VarId]] = {
    arr(for {
      var1 <- ws.allVarIds
      var2 <- ws.allVarIds
      if ws.getDimension(var1) == ws.getDimension(var2)
      if !ws.equalities.testEqual(var1, var2)
      if var1.toString > var2.toString
    } yield js.Array(var1, var2))
  }

  def declareEqual(varId1: VarId, varId2: VarId): JsWorkspace = this.copy(ws = ws.addEquality(varId1, varId2))

  def equalityList: js.Array[js.Array[VarId]] = arr(ws.equalities.sets.map(arr))

  def showVar(varId: VarId): String = LatexString.showVar(varId.varName, ws.getVarSubscript(varId))

  def expressionList: js.Array[VarId] = arr(ws.exprs.keys)
  def addableExpressionList: js.Array[VarId] = arr(ws.allVarIds -- ws.exprs.keys)
  def exprLatex(varId: VarId): String = ws.showExpression(varId)

  def addExpression(varId: VarId): JsWorkspace = this.copy(ws = ws.addExpression(varId))

  private def arr[A](x: Iterable[A]): js.Array[A] = js.Array(x.toSeq : _*)

  def varList: js.Array[VarId] = arr(ws.allVarIds)
}

