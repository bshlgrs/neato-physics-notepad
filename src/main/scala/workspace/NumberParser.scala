package workspace

import scala.util.Try
import scala.util.matching.Regex

object NumberParser {
  def parse(string: String): Try[PhysicalNumber] = {
    Try({
      val (numberPart, unitsPart) = string.splitAt(string.indexWhere(char => !(char == '.' || char.isDigit)))

      val unitRegex = raw"(/)?(\w+)(\^([-\d]+))?".r("divisionSign", "unitString", "unimportantGroup", "exponent")
      val units: List[Dimension] = unitRegex.findAllMatchIn(unitsPart).map({
        case unitRegex(divisionSignOrNull, unitString, _, exponentOrNull) => {
          val exponent = Option(exponentOrNull)
          val divisionSign = Option(divisionSignOrNull)
          knownUnits.getOrElse(unitString, {
            throw new RuntimeException(s"unknown unit $unitString")
          }) ** (exponent.map(_.toInt).getOrElse(1) * (if (divisionSign.isDefined) -1 else 1))
        }
      }).toList

      val dimension = units.reduce(_ * _)
      PhysicalNumber(numberPart.toDouble, dimension)
    })//.toOption
  }

  def knownUnits: Map[String, Dimension] = Map(
    "m" -> Meter,
    "kg" -> Kilogram,
    "s" -> Second,
    "K" -> Kelvin,
    "A" -> Ampere,
    "J" -> Dimension.Joule,
    "N" -> Dimension.Newton,
    "Hz" -> Dimension(Map(Second -> -1))
  )

  def main(args: Array[String]): Unit = {
    println(parse("5 m/s"))
  }
}
