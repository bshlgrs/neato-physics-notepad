package workspace

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}


trait VarId {
  @JSExport
  def varName: String

  def toJsObject: js.Object = this match {
    case EquationVarId(eqIdx, varName) => js.Dynamic.literal("eqIdx" -> eqIdx, "varName" -> varName, "className" -> "EquationVarId")
    case DiagramVarId(diagramIdx, varName) => js.Dynamic.literal("diagramIdx" -> diagramIdx, "varName" -> varName, "className" -> "DiagramVarId")
  }

  def sourceIsGreaterThan(other: VarId): Boolean = {
    // Returns a bool that says which VarId should be ordered first out of this one and the other one.
    (this, other) match {
      case (EquationVarId(x, _), EquationVarId(y, _)) => x > y
      case (DiagramVarId(x, _), DiagramVarId(y, _)) => x > y
      case (DiagramVarId(_, _), EquationVarId(_, _)) => false
      case (EquationVarId(_, _), DiagramVarId(_, _)) => true
    }
  }
}


@JSExportTopLevel("Gem.EquationVarId")
@JSExportAll
case class EquationVarId(eqIdx: Int, varName: String) extends VarId {

}

@JSExportTopLevel("Gem.DiagramVarId")
@JSExportAll
case class DiagramVarId(diagramIdx: Int, varName: String) extends VarId {

}

trait VarIdJs extends js.Object {
  val eqIdx: js.UndefOr[Int]
  val diagramIdx: js.UndefOr[Int]
  val className: String
  val varName: String
}
object VarIdJs {
  def parse(varIdJs: VarIdJs): VarId = varIdJs.className match {
    case "EquationVarId" => EquationVarId(varIdJs.eqIdx.get, varIdJs.varName)
    case "DiagramVarId" => DiagramVarId(varIdJs.diagramIdx.get, varIdJs.varName)
  }
}
