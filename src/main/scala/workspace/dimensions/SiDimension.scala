package workspace.dimensions

import cas.{RationalNumber, RationalNumberJs}
import workspace._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel, ScalaJSDefined}


case class SiDimension(units: Map[SiUnit, RationalNumber[String]])  {
  def *(other: SiDimension): SiDimension = SiDimension(
    (units.keys ++ other.units.keys)
      .map((u) => u -> (this.units.getOrElse(u, RationalNumber[String](0)) + other.units.getOrElse(u, RationalNumber[String](0))).asInstanceOf[RationalNumber[String]])
      .filter(_._2 != RationalNumber(0))
      .toMap
  )

  def /(other: SiDimension): SiDimension = this * (other ** -1)
  def **(other: Int): SiDimension = SiDimension(this.units.mapValues(x => (x * other).asInstanceOf[RationalNumber[String]]))
  def **(other: RationalNumber[String]): SiDimension = SiDimension(this.units.mapValues(n => (n * other).asInstanceOf[RationalNumber[String]]))

  @JSExport
  lazy val toBuckTex: BuckTex = this match {
    case SiDimension.SiJoule => Text("J")
    case SiDimension.SiNewton => Text("N")
    case SiDimension.SiCoulomb => Text("C")
    case SiDimension.SiVolt => Text("V")
    case SiDimension.SiWatt => Text("W")
    case SiDimension.SiOhm => Text("Ω")
    case SiDimension.SiNewtonsPerCoulomb => Text("N/C")
    case _ => CompileToBuckTex.horizontalBox(units.toList.sortBy(_._1.symbol).map({
      case (unit, RationalNumber(1, 1)) => Text(unit.symbol)
      case (unit, power) => CompileToBuckTex.horizontalBox(List(Text(" " + unit.symbol), Sup(List(Text(power.toString)))))
    }))
  }

  lazy val toJsObject: js.Object = js.Dynamic.literal(
    "className" -> "SiDimension", "units" -> js.Dictionary(units.toSeq.map({
      case (siUnit, rationalNumber) =>
        siUnit.symbol -> rationalNumber.toJsObject
    }) :_*)
  )

  lazy val invert: SiDimension = SiDimension(units.mapValues({ case RationalNumber(n, d) => RationalNumber(-n, d) }))
}


trait SiDimensionJs extends js.Object {
  val className: String
  val units: js.Dictionary[RationalNumberJs]
}

object SiDimensionJs {
  def parse(concreteSiDimensionJs: SiDimensionJs): SiDimension = {
    assert(concreteSiDimensionJs.className == "SiDimension")
    SiDimension(
      concreteSiDimensionJs.units.toMap.map({ case (s: String, r: RationalNumberJs) =>
        SiUnit.fromSymbol(s).getOrElse(Util.err("error 123")) -> RationalNumber[String](r.numerator, r.denominator)
      })
    )
  }
}

object SiDimension {
  val SiNewton: SiDimension = Kilogram * Meter / Second / Second
  val SiJoule: SiDimension = SiNewton * Meter
  val SiCoulomb: SiDimension = Ampere * Second
  val SiVolt: SiDimension = SiJoule / SiCoulomb
  val SiNewtonsPerCoulomb: SiDimension = SiNewton / SiCoulomb
  val SiWatt: SiDimension = SiJoule / Second
  val SiOhm: SiDimension = SiVolt / Ampere
  val Dimensionless = SiDimension(Map())
}

case class GeneralUnit(value: Double, dimension: SiDimension, name: UnitName) {
  lazy val toDim: Dimension = Dimension(Map(this -> RationalNumber(1)))
  lazy val toJsObject: js.Object = js.Dynamic.literal(
    "className" -> "GeneralUnit",
    "value" -> value,
    "dimension" -> dimension.toJsObject,
    "name" -> name.toJsObject
  )
}

sealed trait SiUnit {
  def symbol: String = this match {
    case SiUnit.Meter => "m"
    case SiUnit.Kilogram => "kg"
    case SiUnit.Second => "s"
    case SiUnit.Kelvin => "K"
    case SiUnit.Ampere => "A"
  }

  def toJsObject: js.Object = js.Dynamic.literal("className" -> "SiUnit", "symbol" -> this.symbol)
}

object SiUnit {
  case object Meter extends SiUnit
  case object Kilogram extends SiUnit
  case object Second extends SiUnit
  case object Kelvin extends SiUnit
  case object Ampere extends SiUnit

  def fromSymbol(symbol: String): Option[SiUnit] =
    Map("m" -> Meter, "kg" -> Kilogram, "s" -> Second, "K" -> Kelvin, "A" -> Ampere).get(symbol)
}

object Second extends SiDimension(Map(SiUnit.Second -> RationalNumber(1)))
object Meter extends SiDimension(Map(SiUnit.Meter -> RationalNumber(1)))
object Kilogram extends SiDimension(Map(SiUnit.Kilogram -> RationalNumber(1)))
object Kelvin extends SiDimension(Map(SiUnit.Kelvin -> RationalNumber(1)))
object Ampere extends SiDimension(Map(SiUnit.Ampere -> RationalNumber(1)))
