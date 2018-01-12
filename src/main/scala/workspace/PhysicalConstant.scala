package workspace

import cas.{NamedNumber, RationalNumber}
import workspace.dimensions.{Kilogram, Meter, Second}

case class PhysicalConstant(name: String, namedNumber: NamedNumber[String]) {

}

object PhysicalConstant {
  import workspace.dimensions.SiDimension._
  val G = PhysicalConstant("Gravitational constant", NamedNumber(6.67408e-11, "G", (Meter ** 3) / Kilogram / (Second ** 2)))
  val k_E = PhysicalConstant("Coulomb's constant", NamedNumber(8.99e9, "k_E", SiNewton * (Meter ** 2) / (SiCoulomb ** 2)))
  val pi = PhysicalConstant("π", NamedNumber(3.1415926535, "π", Dimensionless))
}
