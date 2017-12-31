package workspace

import cas.{NamedNumber, RationalNumber}

case class PhysicalConstant(name: String, namedNumber: NamedNumber[String]) {

}
object PhysicalConstant {
  import SiDimension._
  val G = PhysicalConstant("Gravitational constant", NamedNumber(6.67408e-11, "G", (Meter ** 3) / Kilogram / (Second ** 2)))
}
