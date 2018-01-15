package workspace.dimensions

import cas.{Expression, ExpressionJs, RationalNumber, RationalNumberJs}
import workspace._

import scala.util.Try
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel, ScalaJSDefined}

case class Dimension(units: Map[GeneralUnit, RationalNumber[String]]) {
  @JSExport
  def totalConstant: Double = units.map({ case (unit: GeneralUnit, power: RationalNumber[String]) => math.pow(unit.value, power.toDouble) }).product

  @JSExport
  def siDimension: SiDimension = units.map({ case (unit: GeneralUnit, power: RationalNumber[String]) => unit.dimension ** power })
    .reduceOption(_ * _)
    .getOrElse(SiDimension.Dimensionless)

  def *(other: Dimension): Dimension = {
    Dimension((units.keys ++ other.units.keys)
      .map((u) => u ->
        (this.units.getOrElse(u, RationalNumber(0)) + other.units.getOrElse(u, RationalNumber(0))).asInstanceOf[RationalNumber[String]]
      )
      .filter(_._2 != RationalNumber(0))
      .toMap)
  }

  def /(other: Dimension): Dimension = this * (other ** -1)

  def **(other: RationalNumber[String]): Dimension = Dimension(this.units.mapValues(n => (n * other).asInstanceOf[RationalNumber[String]]))
  def **(other: Int): Dimension = Dimension(this.units.mapValues(n => (n * RationalNumber[String](other)).asInstanceOf[RationalNumber[String]]))

  @JSExport
  def toBuckTex(value: Double): BuckTex = if (this.units.isEmpty) Text("") else {
    def makeSubscript(num: RationalNumber[String]): Option[BuckTex] = {
      if (num == RationalNumber(1)) None else Some(Sup(List(Text(num.toString))))
    }

    val initialUnitsTex = units.dropRight(1).flatMap({ case (unit: GeneralUnit, num: RationalNumber[String]) =>
      List(Text("·"), Text(unit.name.getString(1))) ++ makeSubscript(num).toList
    })

    val (lastUnit, lastUnitPower) = units.last
    val pluralizedLastUnitName = if (lastUnitPower.toDouble > 0) lastUnit.name.getString(value) else lastUnit.name.getString(1)
    val lastUnitsTex: List[BuckTex] = List(Text("·"), Text(pluralizedLastUnitName)) ++ makeSubscript(lastUnitPower).toList

    CompileToBuckTex.horizontalBox((initialUnitsTex ++ lastUnitsTex).tail.toList)
  }

  def toJsObject: js.Object = js.Dynamic.literal("className" -> "Dimension", "units" -> js.Dictionary(units.map({
    case (u: GeneralUnit, r: RationalNumber[String]) => u.name.string -> r.toJsObject
  }).toSeq :_*))
}



trait DimensionJs extends js.Object {
  val units: js.Dictionary[RationalNumberJs]
}

object DimensionJs {
  def parse(dimensionJs: DimensionJs): Dimension = Dimension(dimensionJs.units.toMap.map({
    case (unitName: String, power: RationalNumberJs) => Dimension.getBySymbol(unitName).get -> RationalNumber[String](power.numerator, power.denominator)
  }))
}

@JSExportTopLevel("Gem.Dimension")
object Dimension {
  val Dimensionless: Dimension = Dimension(Map())
  import SiDimension._

  val minute = GeneralUnit(60, Second, PluralizingUnitName("minute"))
  val hour = GeneralUnit(60 * 60, Second, PluralizingUnitName("hour"))
  val day = GeneralUnit(60 * 60 * 24, Second, PluralizingUnitName("day"))
  val week = GeneralUnit(60 * 60 * 24 * 7, Second, PluralizingUnitName("week"))
  val month = GeneralUnit(60 * 60 * 24 * 30, Second, PluralizingUnitName("month"))
  val year = GeneralUnit(60 * 60 * 24 * 365.25, Second, PluralizingUnitName("year"))
  val kilometer = GeneralUnit(1000, Meter, SymbolUnitName("km"))
  val centimeter = GeneralUnit(0.01, Meter, SymbolUnitName("cm"))
  val millimeter = GeneralUnit(0.001, Meter, SymbolUnitName("mm"))
  val nanometer = GeneralUnit(1e-9, Meter, SymbolUnitName("nm"))
  val lightYear = GeneralUnit(9.4607e15, Meter, PluralizingUnitName("light year"))
  val tonne = GeneralUnit(1e3, Kilogram, PluralizingUnitName("tonne"))
  val parsec = GeneralUnit(3.08567758e16, Meter, PluralizingUnitName("parsec"))
  val meter = GeneralUnit(1, Meter, SymbolUnitName("m"))
  val kilogram = GeneralUnit(1, Kilogram, SymbolUnitName("kg"))
  val second = GeneralUnit(1, Second, SymbolUnitName("s"))
  val degreeKelvin = GeneralUnit(1, Kelvin, SymbolUnitName("°K"))
  val ampere = GeneralUnit(1, Ampere, SymbolUnitName("A"))
  val radian = GeneralUnit(1, Radian, SymbolUnitName("rad"))
  val degree = GeneralUnit(180 / 3.14159265, Radian, SymbolUnitName("rad"))
  val joule = GeneralUnit(1, SiJoule, SymbolUnitName("J"))
  val watt = GeneralUnit(1, SiWatt, SymbolUnitName("W"))
  val electronVolt = GeneralUnit(1.60217662e-19, SiJoule, SymbolUnitName("eV"))
  val newton = GeneralUnit(1, SiNewton, SymbolUnitName("N"))
  val hertz = GeneralUnit(1, Second ** -1, SymbolUnitName("Hz"))
  val ohm = GeneralUnit(1, SiOhm, SymbolUnitName("Ω"))
  val coulomb = GeneralUnit(1, SiCoulomb, SymbolUnitName("C"))
  val volt = GeneralUnit(1, SiVolt, SymbolUnitName("V"))

  lazy val units: Set[(Set[String], GeneralUnit)] = Set(
    Set("min", "mins", "minute", "minutes") -> minute,
    Set("hour", "hours") -> hour,
    Set("day") -> day,
    Set("week") -> week,
    Set("month") -> month,
    Set("year") -> year,
    Set("lightyear", "lightyears") -> lightYear,
    Set("tonne", "tonnes") -> tonne,
    Set("parsec") -> parsec,
    Set("ohm") -> ohm,
    Set("rad", "radian") -> radian,
    Set("degree", "degrees", "°") -> degree,
  ) ++ (for {
    (name: String, unit: GeneralUnit) <- unitsWhichCanHaveSiPrefixes
    (siPrefix: String, multiplier: Double) <- siPrefixes
  } yield (Set(siPrefix + name), GeneralUnit(unit.value * multiplier, unit.dimension, SymbolUnitName(siPrefix + name))))

  val unitsWhichCanHaveSiPrefixes: Set[(String, GeneralUnit)] = Set(
    "m" -> meter,
    "g" -> GeneralUnit(0.001, Kilogram, SymbolUnitName("g")),
    "s" -> second,
    "K" -> degreeKelvin,
    "A" -> ampere,
    "J" -> joule,
    "W" -> watt,
    "eV" -> electronVolt,
    "N" -> newton,
    "Hz" -> hertz,
    "C" -> coulomb,
    "V" -> GeneralUnit(1, SiVolt, SymbolUnitName("V"))
  )

  val siPrefixes: Set[(String, Double)] = Set(
    "a" -> 1e-18,
    "f" -> 1e-15,
    "p" -> 1e-12,
    "n" -> 1e-9,
    "μ" -> 1e-6,
    "m" -> 1e-3,
    "c" -> 0.01,
    "" -> 1.0,
    "k" -> 1e3,
    "M" -> 1e6,
    "G" -> 1e9,
    "T" -> 1e12,
    "P" -> 1e15,
    "E" -> 1e18,
  )

  def getBySymbol(symbol: String): Option[GeneralUnit] = Dimension.units.find(_._1.contains(symbol)).map(_._2)

  @JSExport
  def parseJs(str: String): Dimension = parse(str).getOrElse(null)

  def parse(str: String): Try[Dimension] = Try({
    val unitRegex = raw"(/)?(\w+)(\^([-\d]+))?".r("divisionSign", "unitString", "unimportantGroup", "exponent")
    val matches = unitRegex.findAllMatchIn(str).map({
      case unitRegex(divisionSignOrNull, unitString, _, exponentOrNull) =>
        val exponent = Option(exponentOrNull)
        val divisionSign = Option(divisionSignOrNull)
        getBySymbol(unitString).getOrElse({
          throw new RuntimeException(s"unknown unit $unitString")
        }).toDim ** (
          exponent.map((x) => RationalNumber[String](x.toInt)).getOrElse(RationalNumber[String](1))
          * (if (divisionSign.isDefined) RationalNumber[String](-1) else RationalNumber[String](1))).asInstanceOf[RationalNumber[String]]
    })

    matches.reduceOption(_ * _).getOrElse(Dimension.Dimensionless)
  })
}
