package workspace

import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}
import scala.util.Try

@JSExportAll
case class PhysicalNumber(value: Double, dimension: Dimension) {
  @JSExport
  lazy val toBuckTex: BuckTex = CompileToBuckTex.horizontalBox(List(Text(value.toString), dimension.toBuckTex))
}

case class NonSiUnit(value: Double, dimension: Dimension, name: String)

object NonSiUnit {
  val nonSiUnits: Set[(Set[String], NonSiUnit)] = Set(
    Set("min", "mins", "minute") -> NonSiUnit(60, Second, "min"),
    Set("hour", "hours") -> NonSiUnit(60 * 60, Second, "hr"),
    Set("day") -> NonSiUnit(60 * 60, Second, "days"),
    Set("km") -> NonSiUnit(1000, Meter, "km"),
    Set("cm") -> NonSiUnit(0.01, Meter, "cm"),
    Set("mm") -> NonSiUnit(0.001, Meter, "mm"),
    Set("nm") -> NonSiUnit(1e-9, Meter, "nm"),
    Set("lightyear") -> NonSiUnit(9.4607e15, Meter, "light year"),
    Set("parsec") -> NonSiUnit(3.08567758e16, Meter, "parsec"),
  )
}

@JSExportTopLevel("Gem.PhysicalNumber")
object PhysicalNumber {
  def parsePhysicalNumber(string: String): Try[PhysicalNumber] = for {
    (numberPart, unitsPart) <- Try(string.splitAt(string.indexWhere(char => !(".e-".contains(char) || char.isDigit))))
    number <- Try(numberPart.toDouble)
    dimension <- Dimension.parse(unitsPart)
  } yield PhysicalNumber(number, dimension)

  @JSExport("parsePhysicalNumber")
  def parsePhysicalNumberJs(string: String): PhysicalNumber = parsePhysicalNumber(string).getOrElse(null)
}
