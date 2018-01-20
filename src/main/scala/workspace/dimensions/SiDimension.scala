package workspace.dimensions

import cas.{RationalNumber, RationalNumberJs}
import workspace._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel, ScalaJSDefined}


sealed trait SiDimension {

  val units: Map[SiUnit, RationalNumber[String]]

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

  @JSExport
  def equalUnits(other: SiDimension): Boolean = this.units == other.units

  def toJsObject: js.Object
}

case class ConcreteSiDimension(units: Map[SiUnit, RationalNumber[String]]) extends SiDimension {
  def toJsObject: js.Object = js.Dynamic.literal(
    "className" -> "ConcreteSiDimension", "units" -> js.Dictionary(units.toSeq.map({
      case (siUnit, rationalNumber) =>
        siUnit.symbol -> rationalNumber.toJsObject
    }) :_*)
  )
}


trait ConcreteSiDimensionJs extends js.Object {
  val className: String
  val units: js.Dictionary[RationalNumberJs]
}

object ConcreteSiDimensionJs {
  def parse(concreteSiDimensionJs: ConcreteSiDimensionJs): ConcreteSiDimension = {
    assert(concreteSiDimensionJs.className == "ConcreteSiDimension")
    ConcreteSiDimension(
      concreteSiDimensionJs.units.toMap.map({ case (s: String, r: RationalNumberJs) =>
        SiUnit.fromSymbol(s).getOrElse(Util.err("error 123")) -> RationalNumber[String](r.numerator, r.denominator)
      })
    )
  }
}

object SiDimension {
  val SiNewton: SiDimension = SiDimension.fromInts(Map[SiUnit, Int](Kilogram -> 1, Meter -> 1, Second -> -2))
  val SiJoule: SiDimension = SiNewton * Meter
  val SiCoulomb: SiDimension = Ampere * Second
  val SiVolt: SiDimension = SiJoule / SiCoulomb
  val SiNewtonsPerCoulomb: SiDimension = SiNewton / SiCoulomb
  val SiWatt: SiDimension = SiJoule / Second
  val SiOhm: SiDimension = SiVolt / Ampere
  val Dimensionless = ConcreteSiDimension(Map())

  def apply(units: Map[SiUnit, RationalNumber[String]]): SiDimension = ConcreteSiDimension(units)
  def fromInts(units: Map[SiUnit, Int]): SiDimension = ConcreteSiDimension(units.mapValues(int => RationalNumber[String](int)))
}

case class GeneralUnit(value: Double, dimension: SiDimension, name: UnitName) {
  def toDim: Dimension = Dimension(Map(this -> RationalNumber(1)))
  def toJsObject: js.Object = js.Dynamic.literal(
    "className" -> "GeneralUnit",
    "value" -> value,
    "dimension" -> dimension.toJsObject,
    "name" -> name.toJsObject
  )
}

sealed trait SiUnit extends SiDimension {
  val units = Map(this -> RationalNumber(1))

  def symbol: String = this match {
    case Meter => "m"
    case Kilogram => "kg"
    case Second => "s"
    case Kelvin => "K"
    case Ampere => "A"
  }

  override def toJsObject: js.Object = js.Dynamic.literal("className" -> "SiUnit", "symbol" -> this.symbol)
}

object SiUnit {
  def fromSymbol(symbol: String): Option[SiUnit] =
    Map("m" -> Meter, "kg" -> Kilogram, "s" -> Second, "K" -> Kelvin, "A" -> Ampere).get(symbol)
}

case object Meter extends SiUnit
case object Kilogram extends SiUnit
case object Second extends SiUnit
case object Kelvin extends SiUnit
case object Ampere extends SiUnit
