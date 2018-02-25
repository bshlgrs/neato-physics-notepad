package cas

// copied from https://gist.github.com/stoyanr/4967570
import scala.math._

case class ComplexNumber(re: Double, im: Double) {
  private val modulus = sqrt(pow(re, 2) + pow(im, 2))
  val theta: Double = Math.atan2(im, re)

  def this(re: Double) = this(re, 0)

  def unary_+ : ComplexNumber = this
  def unary_- = new ComplexNumber(-re, -im)
  def unary_~ = new ComplexNumber(re, -im) // conjugate
  def unary_! : Double = modulus

  def +(c: ComplexNumber): ComplexNumber = new ComplexNumber(re + c.re, im + c.im)
  def -(c: ComplexNumber): ComplexNumber = this + -c
  def *(c: ComplexNumber): ComplexNumber =
    new ComplexNumber(re * c.re - im * c.im, im * c.re + re * c.im)
  def /(c: ComplexNumber): ComplexNumber = {
    require(c.re != 0 || c.im != 0)
    val d = pow(c.re, 2) + pow(c.im, 2)
    new ComplexNumber((re * c.re + im * c.im) / d, (im * c.re - re * c.im) / d)
  }



  def **(other: ComplexNumber): ComplexNumber = {
    require(other.im == 0)
    if (this.im == 0) {
      ComplexNumber.fromModulusAndAngle(Math.pow(modulus, other.re), theta * other.re)
    } else {
      throw new RuntimeException(s"Can't raise $this to $other")
    }
  }

  def getAsReal: Double = {
    require(Math.abs(im / re) < 1e-8, s"imaginary component must be zero, but it was $im")
    re
  }

  override def toString: String =
    this match {
      case ComplexNumber.i => "i"
      case ComplexNumber(re, 0) => re.toString
      case ComplexNumber(0, im) => im.toString + "*i"
      case _ => asString
    }
  private def asString =
    re + (if (im < 0) "-" + -im else "+" + im) + "*i"
}

object ComplexNumber extends Numeric[ComplexNumber] {
  // Constants
  val i = new ComplexNumber(0, 1)

  // Factory methods
  def apply(re: Double) = new ComplexNumber(re)

  def fromModulusAndAngle(modulus: Double, angle: Double) = ComplexNumber(modulus * Math.cos(angle), modulus * Math.sin(angle))

  // Implicit conversions
  implicit def fromDouble(d: Double) = new ComplexNumber(d)
  implicit def fromFloat(f: Float) = new ComplexNumber(f)
  implicit def fromLong(l: Long) = new ComplexNumber(l)
  implicit def fromInt2(i: Int) = new ComplexNumber(i)
  implicit def fromShort(s: Short) = new ComplexNumber(s)

  def toDouble(x: ComplexNumber): Double = x.getAsReal
  def toFloat(x: ComplexNumber): Float = x.getAsReal.toFloat
  def toLong(x: ComplexNumber): Long = x.getAsReal.toLong
  def toInt(x: ComplexNumber): Int = x.getAsReal.toInt
  def fromInt(x: Int): ComplexNumber = ComplexNumber(x)
  // Numeric requires these
  def plus(x: ComplexNumber, y: ComplexNumber): ComplexNumber = x + y
  def minus(x: ComplexNumber, y: ComplexNumber): ComplexNumber = x - y
  def times(x: ComplexNumber, y: ComplexNumber): ComplexNumber = x * y
  def negate(x: ComplexNumber): ComplexNumber = -x
  def divide(x: ComplexNumber, y: ComplexNumber): ComplexNumber = x / y
  def compare(x: ComplexNumber, y: ComplexNumber): Int = !x compare !y
}

import ComplexNumber._
