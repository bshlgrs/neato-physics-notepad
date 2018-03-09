package cas

object RationalNumber {
  def zero = RationalNumber(0)
  def one = RationalNumber(1)
  def two = RationalNumber(2)
  def half = RationalNumber(1, 2)

  def build[A](numerator: Int, denominator: Int): RationalNumber[A] = {
    assert(denominator != 0, "denominator is zero")
    if (denominator < 0)
      build(-numerator, -denominator)
    else {
      val gcd = Expression.euclidsAlgorithm(math.abs(numerator), math.abs(denominator))
      assert(denominator > 0, "gcd algorithm is borked")
      RationalNumber(numerator / gcd, denominator / gcd)
    }
  }

  def makeFromDouble[A](double: Double): Option[RationalNumber[A]] = {
    (for {
      i <- Range(1, 10) // inclusive
      maybeNum = double * i
      if math.abs(maybeNum - maybeNum.round) < 0.0001
    } yield RationalNumber[A](maybeNum.round.toInt, i)).headOption
  }
}

case class RationalNumber[A](numerator: Int, denominator: Int = 1) extends Constant[A] {
  assert(denominator > 0, "denominator must be > 0")

  def toDouble: Double = numerator.toDouble / denominator
}
