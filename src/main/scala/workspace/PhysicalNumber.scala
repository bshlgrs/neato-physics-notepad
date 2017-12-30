package workspace

import scala.scalajs.js.annotation.{JSExport, JSExportAll, JSExportTopLevel}
import scala.util.Try

@JSExportAll
case class PhysicalNumber(value: Double, siDimension: SiDimension, originalInput: Option[(Double, Dimension)]) {
  @JSExport
  lazy val toBuckTex: BuckTex = originalInput match {
    case None => CompileToBuckTex.horizontalBox(List(Text(value.toString + " "), siDimension.toBuckTex))
    case Some((originalValue, originalDimension)) => CompileToBuckTex.horizontalBox(List(Text(originalValue.toString + " "), originalDimension.toBuckTex(originalValue)))
  }
//  lazy val toBuckTex: BuckTex = CompileToBuckTex.horizontalBox(List(Text(value.toString), siDimension.toBuckTex))

  // TODO: some method that outputs it in its original units
}

@JSExportTopLevel("Gem.PhysicalNumber")
object PhysicalNumber {
  def parsePhysicalNumber(string: String): Try[PhysicalNumber] = for {
    (numberPart, unitsPart) <- Try(string.splitAt(string.indexWhere(char => !(".e-".contains(char) || char.isDigit))))
    number <- Try(numberPart.toDouble)
    dimension <- Dimension.parse(unitsPart)
  } yield PhysicalNumber(number * dimension.totalConstant, dimension.siDimension, Some(number, dimension))

  @JSExport("parsePhysicalNumber")
  def parsePhysicalNumberJs(string: String): PhysicalNumber = parsePhysicalNumber(string).getOrElse(null)

  def apply(value: Double, siDimension: SiDimension): PhysicalNumber = PhysicalNumber(value, siDimension, None)
}
