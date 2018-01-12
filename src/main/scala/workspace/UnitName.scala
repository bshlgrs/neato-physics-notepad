package workspace

import scala.scalajs.js


sealed trait UnitName {
  def getString(num: Double): String
  def toJsObject: js.Object
}
case class SymbolUnitName(symbol: String) extends UnitName {
  def getString(x: Double): String = symbol
  override def toJsObject: js.Object = js.Dynamic.literal("className" -> "SymbolUnitName", "symbol" -> this.symbol)
}
case class PluralizingUnitName(name: String) extends UnitName {
  def getString(x: Double): String = if (x == 1.0) name else name + "s"
  override def toJsObject: js.Object = js.Dynamic.literal("className" -> "SymbolUnitName", "name" -> this.name)
}
