package workspace

import cas.{NamedNumber, RationalNumber}
import workspace.dimensions._

case class PhysicalConstant(name: String, namedNumber: NamedNumber[String]) {

}

object PhysicalConstant {
  import workspace.dimensions.SiDimension._
  val G = PhysicalConstant("Gravitational constant", NamedNumber(6.67408e-11, "G", (Meter ** 3) / Kilogram / (Second ** 2)))
  val k_E = PhysicalConstant("Coulomb's constant", NamedNumber(8.99e9, "k_E", SiNewton * (Meter ** 2) / (SiCoulomb ** 2)))
  val pi = PhysicalConstant("π", NamedNumber(3.1415926535, "π", Dimensionless))
  val hbar = PhysicalConstant("ħ", NamedNumber(1.0545718e-34, "ħ", Meter ** 2 * Kilogram / Second))
  val plankConstant = PhysicalConstant("h", NamedNumber(6.626070040e-34, "h", Meter ** 2 * Kilogram / Second))
  val speedOfLight = PhysicalConstant("c", NamedNumber(299792458, "c", Meter / Second))
  val vacuumPermeability = PhysicalConstant("μ_0", NamedNumber(1.256637e-6, "μ_0", SiNewton / (Ampere ** 2)))
  val vacuumPermittivity = PhysicalConstant("ε_0", NamedNumber(8.854187817e-12, "ε_0", SiFarad / Meter))
  val elementaryCharge = PhysicalConstant("e", NamedNumber(1.6021766208e-19, "e", SiCoulomb))
  val boltzmannConstant = PhysicalConstant("k", NamedNumber(1.3806485e-23, "k", SiJoule / Kelvin))
  val avogadroConstant = PhysicalConstant("N_A", NamedNumber(6.022140858e23, "N_A", Mole.invert))
  val faradayConstant = PhysicalConstant("F", NamedNumber(96485.333, "F", SiCoulomb / Mole))

  val constants: Set[PhysicalConstant] = Set(G, k_E, pi, hbar, plankConstant, speedOfLight)
}
