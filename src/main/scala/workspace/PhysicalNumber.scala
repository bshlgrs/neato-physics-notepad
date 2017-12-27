package workspace

import scala.scalajs.js.annotation.JSExport

case class PhysicalNumber(value: Double, dimension: Dimension) {
  @JSExport
  lazy val toDisplayMath: DisplayMath = DisplayMath(value.toString) ++ dimension.toDisplayMath
}
