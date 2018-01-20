package workspace

import workspace.dimensions.{ConcreteSiDimensionJs, Dimension, DimensionJs, SiDimension}

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel, ScalaJSDefined}
import scala.util.Try

@JSExportAll
case class PhysicalNumber(value: Double, siDimension: SiDimension, originalInput: Option[(Double, Dimension)], name: Option[String] = None) {
  lazy val toBuckTex: BuckTex = originalInput match {
    case None => siUnitToBuckTex
    case Some((originalValue, originalDimension)) => CompileToBuckTex.horizontalBox(List(Text("%.4g".format(originalValue) + " "), originalDimension.toBuckTex(originalValue)))
  }

  lazy val siUnitToBuckTex: BuckTex = CompileToBuckTex.horizontalBox(List(Text("%.4g".format(value) + " "), siDimension.toBuckTex))

  def valueInOtherUnits(dimension: Dimension): Double = {
    assert(dimension.siDimension == siDimension)
    value / dimension.totalConstant
  }

  def changeDimension(dim: Dimension): PhysicalNumber = {
    this.copy(originalInput = Some(value / dim.totalConstant, dim))
  }

  def toJsObject: js.Object = originalInput match {
    case Some((numValue, dim)) =>
      js.Dynamic.literal("value" -> value, "siDimension" -> siDimension.toJsObject,
        "originalInputValue" -> numValue, "originalInputDim" -> dim.toJsObject)
    case None => js.Dynamic.literal("value" -> value, "siDimension" -> siDimension.toJsObject)
  }
}

trait PhysicalNumberJs extends js.Object {
  val value: Double
  val siDimension: ConcreteSiDimensionJs
  val originalInputValue: js.UndefOr[Double]
  val originalInputDim: js.UndefOr[DimensionJs]
}

object PhysicalNumberJs {
  def parse(physicalNumberJs: PhysicalNumberJs): PhysicalNumber = PhysicalNumber(
    physicalNumberJs.value,
    ConcreteSiDimensionJs.parse(physicalNumberJs.siDimension),
    physicalNumberJs.originalInputValue.toOption match {
      case None => None
      case Some(value) => Some(value -> DimensionJs.parse(physicalNumberJs.originalInputDim.toOption.getOrElse(
        throw new RuntimeException(s"Don't know how to parse originalInputDim in $physicalNumberJs"))))
    })
}

@JSExportTopLevel("Gem.PhysicalNumber")
object PhysicalNumber {
  def parsePhysicalNumber(string: String): Try[PhysicalNumber] = {
    val predicate = (char: Char) => !(".e-".contains(char) || char.isDigit)
    for {
      (numberPart, unitsPart) <- Try({
        if (string.exists(predicate))
          string.splitAt(string.indexWhere(predicate))
        else
          // You have to special case this because indexWhere returns -1 if the thing isn't found.
          (string, "")
      })
      number <- Try(numberPart.toDouble)
      dimension <- Dimension.parse(unitsPart)  // if (unitsPart.length > 0) Dimension.parse(unitsPart) else Try(Dimension.Dimensionless)
    } yield {
      PhysicalNumber(number * dimension.totalConstant, dimension.siDimension, Some(number, dimension))
    }
  }

  @JSExport("parsePhysicalNumber")
  def parsePhysicalNumberJs(string: String): PhysicalNumber = parsePhysicalNumber(string).getOrElse(null)

  @JSExport("apply")
  def apply(value: Double, siDimension: SiDimension): PhysicalNumber = PhysicalNumber(value, siDimension, None)

  @JSExport("applyWithDimension")
  def applyWithDimension(value: Double, dimension: Dimension): PhysicalNumber =
    PhysicalNumber(value * dimension.totalConstant, dimension.siDimension, Some(value, dimension), None)
}
