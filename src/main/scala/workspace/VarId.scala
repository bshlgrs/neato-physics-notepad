package workspace

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportTopLevel("Gem.VarId")
@JSExportAll
case class VarId(eqIdx: Int, varName: String) {
  def toJsObject: js.Object = js.Dynamic.literal("eqIdx" -> eqIdx, "varName" -> varName)
}

trait VarIdJs extends js.Object {
  val eqIdx: Int; val varName: String

}
object VarIdJs {
  def parse(varIdJs: VarIdJs): VarId = VarId(varIdJs.eqIdx, varIdJs.varName)
}
