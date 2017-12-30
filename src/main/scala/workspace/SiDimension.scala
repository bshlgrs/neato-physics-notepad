package workspace

import workspace.SiDimension.{SiCoulomb, SiJoule, SiNewton, SiOhm}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.Try

trait SiDimension {

  val units: Map[SiUnit, Int]

  def *(other: SiDimension): SiDimension = SiDimension(
    (units.keys ++ other.units.keys)
      .map((u) => u -> (this.units.getOrElse(u, 0) + other.units.getOrElse(u, 0)))
      .filter(_._2 != 0)
      .toMap
  )

  def /(other: SiDimension): SiDimension = SiDimension(
    (units.keys ++ other.units.keys)
      .map((u) => u -> (this.units.getOrElse(u, 0) - other.units.getOrElse(u, 0)))
      .filter(_._2 != 0)
      .toMap
  )

  def **(other: Int): SiDimension = SiDimension(this.units.mapValues(_ * other))

  @JSExport
  lazy val toBuckTex: BuckTex = this match {
    case SiDimension.SiJoule => Text("J")
    case SiDimension.SiNewton => Text("N")
    case SiDimension.SiCoulomb => Text("C")
    case _ => CompileToBuckTex.horizontalBox(units.toList.sortBy(_._1.symbol).map({
      case (unit, 1) => Text(unit.symbol)
      case (unit, power) => CompileToBuckTex.horizontalBox(List(Text(" " + unit.symbol), Sup(List(Text(power.toString)))))
    }))
  }

  @JSExport
  def equalUnits(other: SiDimension): Boolean = this.units == other.units
}

case class ConcreteSiDimension(units: Map[SiUnit, Int]) extends SiDimension


object SiDimension {
  val SiNewton = SiDimension(Map(Kilogram -> 1, Meter -> 1, Second -> -2))
  val SiJoule: SiDimension = SiNewton * Meter
  val SiCoulomb: SiDimension = Ampere * Second
  val Volt: SiDimension = SiJoule / SiCoulomb
  val Watt: SiDimension = SiJoule / Second
  val SiOhm: SiDimension = Volt / Ampere
  val Dimensionless = ConcreteSiDimension(Map())

  def apply(units: Map[SiUnit, Int]): SiDimension = ConcreteSiDimension(units)
}

case class Unit(value: Double, dimension: SiDimension, name: UnitName) {
  def toDim: Dimension = Dimension(Map(this -> 1))
}



case class Dimension(units: Map[Unit, Int]) {
  def totalConstant: Double = units.map({ case (unit: Unit, power: Int) => math.pow(unit.value, power) }).product

  def siDimension: SiDimension = units.map({ case (unit: Unit, power: Int) => unit.dimension ** power })
    .reduceOption(_ * _)
    .getOrElse(SiDimension.Dimensionless)

  def *(other: Dimension): Dimension = {
    Dimension((units.keys ++ other.units.keys)
      .map((u) => u -> (this.units.getOrElse(u, 0) + other.units.getOrElse(u, 0)))
      .filter(_._2 != 0)
      .toMap)
  }

  def /(other: Dimension): Dimension = this * (other ** -1)

  def **(other: Int): Dimension = Dimension(this.units.mapValues(_ * other))
}

object Dimension {
  val Dimensionless: Dimension = Dimension(Map())

  val minute = Unit(60, Second, PluralizingUnitName("minute"))
  val hour = Unit(60 * 60, Second, PluralizingUnitName("hour"))
  val day = Unit(60 * 60 * 24, Second, PluralizingUnitName("day"))
  val kilometer = Unit(1000, Meter, SymbolUnitName("km"))
  val centimeter = Unit(0.01, Meter, SymbolUnitName("cm"))
  val millimeter = Unit(0.001, Meter, SymbolUnitName("mm"))
  val nanometer = Unit(1e-9, Meter, SymbolUnitName("nm"))
  val lightYear = Unit(9.4607e15, Meter, PluralizingUnitName("light year"))
  val parsec = Unit(3.08567758e16, Meter, PluralizingUnitName("parsec"))
  val meter = Unit(1, Meter, SymbolUnitName("m"))
  val kilogram = Unit(1, Kilogram, SymbolUnitName("kg"))
  val second = Unit(1, Second, SymbolUnitName("s"))
  val degreeKelvin = Unit(1, Kelvin, SymbolUnitName("°K"))
  val ampere = Unit(1, Ampere, SymbolUnitName("A"))
  val joule = Unit(1, SiJoule, SymbolUnitName("J"))
  val newton = Unit(1, SiNewton, SymbolUnitName("N"))
  val hertz = Unit(1, Second ** -1, SymbolUnitName("Hz"))
  val ohm = Unit(1, SiOhm, SymbolUnitName("Ω"))
  val coulomb = Unit(1, SiCoulomb, SymbolUnitName("C"))

  val units: Set[(Set[String], Unit)] = Set(
    Set("min", "mins", "minute", "minutes") -> minute,
    Set("hour", "hours") -> hour,
    Set("day") -> day,
    Set("km") -> kilometer,
    Set("cm") -> centimeter,
    Set("mm") -> millimeter,
    Set("nm") -> nanometer,
    Set("lightyear", "lightyears") -> lightYear,
    Set("parsec") -> parsec,
    Set("m") -> meter,
    Set("kg") -> kilogram,
    Set("s") -> second,
    Set("K") -> degreeKelvin,
    Set("A") -> ampere,
    Set("J") -> joule,
    Set("N") -> newton,
    Set("Hz") -> hertz,
    Set("ohm") -> ohm,
    Set("C") -> coulomb
  )

  def parse(str: String): Try[Dimension] = Try({
    val unitRegex = raw"(/)?(\w+)(\^([-\d]+))?".r("divisionSign", "unitString", "unimportantGroup", "exponent")
    unitRegex.findAllMatchIn(str).map({
      case unitRegex(divisionSignOrNull, unitString, _, exponentOrNull) =>
        val exponent = Option(exponentOrNull)
        val divisionSign = Option(divisionSignOrNull)
        Dimension.units.find(_._1.contains(unitString)).map(_._2).getOrElse({
          throw new RuntimeException(s"unknown unit $unitString")
        }).toDim ** (exponent.map(_.toInt).getOrElse(1) * (if (divisionSign.isDefined) -1 else 1))
    }).reduce(_ * _)
  })
}

sealed trait SiUnit extends SiDimension {
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

trait UnitName {
  def getString(num: Double): String
}
case class SymbolUnitName(symbol: String) extends UnitName {
  def getString(x: Double): String = symbol
}
case class PluralizingUnitName(name: String) extends UnitName {
  def getString(x: Double): String = if (x == 1.0) name else name + "s"
}
