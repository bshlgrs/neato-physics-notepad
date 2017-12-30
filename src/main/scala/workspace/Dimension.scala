package workspace

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.Try

trait Dimension {

  val units: Map[SiUnit, Int]

  def *(other: Dimension): Dimension = Dimension(
    (units.keys ++ other.units.keys)
      .map((u) => u -> (this.units.getOrElse(u, 0) + other.units.getOrElse(u, 0)))
      .filter(_._2 != 0)
      .toMap
  )

  def /(other: Dimension): Dimension = Dimension(
    (units.keys ++ other.units.keys)
      .map((u) => u -> (this.units.getOrElse(u, 0) - other.units.getOrElse(u, 0)))
      .filter(_._2 != 0)
      .toMap
  )

  def **(other: Int): Dimension = Dimension(this.units.mapValues(_ * other))

  @JSExport
  lazy val toBuckTex: BuckTex = this match {
    case Dimension.Joule => Text("J")
    case Dimension.Newton => Text("N")
    case _ => CompileToBuckTex.horizontalBox(units.toList.sortBy(_._1.symbol).map({
      case (unit, 1) => Text(unit.symbol)
      case (unit, power) => CompileToBuckTex.horizontalBox(List(Text(" " + unit.symbol), Sup(List(Text(power.toString)))))
    }))
  }

  @JSExport
  def equalUnits(other: Dimension): Boolean = this.units == other.units
}

case class ConcreteDimension(units: Map[SiUnit, Int]) extends Dimension

@JSExportTopLevel("Gem.Dimension")
object Dimension {
  val Newton = Dimension(Map(Kilogram -> 1, Meter -> 1, Second -> -2))
  val Joule: Dimension = Newton * Meter
  var Coulomb: Dimension = Ampere * Second
  var Volt: Dimension = Joule / Coulomb
  var Watt: Dimension = Joule / Second
  var Ohm: Dimension = Volt / Ampere
  var Dimensionless = ConcreteDimension(Map())


  def apply(units: Map[SiUnit, Int]): Dimension = ConcreteDimension(units)
  def parse(str: String): Try[Dimension] = Try({
    val unitRegex = raw"(/)?(\w+)(\^([-\d]+))?".r("divisionSign", "unitString", "unimportantGroup", "exponent")
    unitRegex.findAllMatchIn(str).map({
      case unitRegex(divisionSignOrNull, unitString, _, exponentOrNull) => {
        val exponent = Option(exponentOrNull)
        val divisionSign = Option(divisionSignOrNull)
        symbolMap.getOrElse(unitString, {
          throw new RuntimeException(s"unknown unit $unitString")
        }) ** (exponent.map(_.toInt).getOrElse(1) * (if (divisionSign.isDefined) -1 else 1))
      }
    }).reduce(_ * _)
  })

  def parsePhysicalNumber(string: String): Try[PhysicalNumber] = for {
    (numberPart, unitsPart) <- Try(string.splitAt(string.indexWhere(char => !(char == '.' || char.isDigit))))
    number <- Try(numberPart.toDouble)
    dimension <- Dimension.parse(unitsPart)
  } yield PhysicalNumber(number, dimension)

  @JSExport("parsePhysicalNumber")
  def parsePhysicalNumberJs(string: String): PhysicalNumber = parsePhysicalNumber(string).getOrElse(null)

  val symbolMap: Map[String, Dimension] = Map(
    "m" -> Meter,
    "kg" -> Kilogram,
    "s" -> Second,
    "K" -> Kelvin,
    "A" -> Ampere,
    "J" -> Joule,
    "N" -> Newton,
    "Hz" -> (Second ** -1),
    "ohm" -> Ohm
  )
}

sealed trait SiUnit extends Dimension {
  val units = Map(this -> 1)

  def symbol: String = this match {
    case Meter => "m"
    case Kilogram => "kg"
    case Second => "s"
    case Kelvin => "K"
    case Ampere => "A"
  }
}
case object Meter extends SiUnit
case object Kilogram extends SiUnit
case object Second extends SiUnit
case object Kelvin extends SiUnit
case object Ampere extends SiUnit
