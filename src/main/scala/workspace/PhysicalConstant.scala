package workspace

import cas.RationalNumber

case class PhysicalConstant(name: String, symbol: String, value: Double, siDimension: SiDimension) {

}
object PhysicalConstant {
  import SiDimension._
  val G = PhysicalConstant("Gravitational constant", "G", 6.67408e-11, SiNewton * (Meter ** 2) / (Kilogram ** 2))
}
