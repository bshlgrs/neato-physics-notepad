package cas

trait Expression[A] {
  override def toString: String = this.toStringWithBinding._1

  def toStringWithBinding: (String, Int) = {
    def wrap(tuple: (String, Int), binding: Int) = if (tuple._2 >= binding) tuple._1 else s"(${tuple._1})"

    this match {
      case Sum(set) => set.map((x) => wrap(x.toStringWithBinding, 0)).mkString(" + ") -> 0
      case Product(set) => set.map((x) => wrap(x.toStringWithBinding, 1)).mkString(" * ") -> 1
      case Power(lhs, rhs) => wrap(lhs.toStringWithBinding, 2) + "^" + wrap(rhs.toStringWithBinding, 2) -> 2
      case Variable(thing) => thing.toString -> 3
      case RealNumber(r) => r.toString -> 3
      case RationalNumber(n, 1) => n.toString -> 3
      case RationalNumber(n, d) => s"$n/$d" -> 1
      case _ => ???
    }
  }

  def solve(x: A): List[Expression[A]] = {
    ???
  }

  def sqrt: Expression[A] = this ** RationalNumber(1, 2)

  def solve(x: Variable[A]): List[Expression[A]] = this.solve(x.thing)

  def subs(x: A): Expression[A] = ???

  def +(other: Expression[A]): Expression[A] = (this, other) match {
    case (RationalNumber(n1, d1), RationalNumber(n2, d2)) => {
      val numerator = n1 * d2 + n2 * d1
      val denominator = d1 * d2
      val gcd = Expression.euclidsAlgorithm(numerator, denominator)
      RationalNumber(numerator / gcd, denominator / gcd)
    }
    case (Sum(lhs), Sum(rhs)) => {
      val lhsTermTuples = lhs.map(_.asTerm).toMap
      val rhsTermTuples = rhs.map(_.asTerm).toMap

      val termTuples: Iterable[(Expression[A], Expression[A])] = (lhsTermTuples.keys ++ rhsTermTuples.keys).map((term: Expression[A]) =>
        term -> (lhsTermTuples.getOrElse(term, RationalNumber[A](0)) + rhsTermTuples.getOrElse(term, RationalNumber[A](0)))
      ).filter(_._2 != RationalNumber(0))

      // now we have a bunch of terms with the thing on the left and the coefficient on the right
      val terms = termTuples.map({ case (x, y) => x * y }).toSet

      terms.size match {
        case 0 => RationalNumber(0)
        case 1 => terms.head
        case _ => Sum(terms)
      }
    }
    case (Sum(lhs), rhs) => {
      Sum(lhs) + Sum(Set(rhs))
    }
    case (lhs, Sum(rhs)) => {
      Sum(Set(lhs)) + Sum(rhs)
    }
    case _ => Sum(Set(this)) + Sum(Set(other))
  }

  def *(other: Expression[A]): Expression[A] = (this, other) match {
    case (RationalNumber(n1, d1), RationalNumber(n2, d2)) => {
      val numerator = n1 * n2
      val denominator = d1 * d2
      val gcd = Expression.euclidsAlgorithm(numerator, denominator)
      RationalNumber(numerator / gcd, denominator / gcd)
    }
    case (Product(lhs), Product(rhs)) => {
      val lhsFactorTuples = lhs.map(_.asFactor).toMap
      val rhsFactorTuples = rhs.map(_.asFactor).toMap

      val factorTuples: Iterable[(Expression[A], Expression[A])] = (lhsFactorTuples.keys ++ rhsFactorTuples.keys).map((factor: Expression[A]) =>
        factor -> (lhsFactorTuples.getOrElse(factor, RationalNumber[A](0)) + rhsFactorTuples.getOrElse(factor, RationalNumber[A](0)))
      ).filter(_._1 != RationalNumber(1))

      val factors = factorTuples.map({ case (x, y) => x ** y}).toSet

      if (factors.contains(RationalNumber(0))) RationalNumber(0) else Expression.makeProduct(factors)
    }
    case (_: Product[_], _) => this * Product(Set(other))
    case (_, _: Product[_]) => Product(Set(this)) * other
    case (_, _) => Product(Set(this)) * Product(Set(other))
  }

  def -(other: Expression[A]): Expression[A] = this + RationalNumber(-1) * other

  def /(other: Expression[A]): Expression[A] = this * (other ** RationalNumber(-1))

  def **(other: Expression[A]): Expression[A] = (this, other) match {
    case (RationalNumber(1, 1), _) => RationalNumber(1)
    case (RationalNumber(0, 1), _) => RationalNumber(0)
    case (_, RationalNumber(1, 1)) => this
    case (_, RationalNumber(0, 1)) => RationalNumber(1)
    case _ => Power(this, other)
  }

  protected def asTerm: (Expression[A], Constant[A]) = {
    // 2*x -> (x, 2)
    // 2*x^2*y -> (x^2*y, 2)
    // 2 -> (1, 2)
    // 5/2 -> (1, 5/2)
    // x + y -> (x + y, 1)
    this match {
      case Product(factors) => {
        val const = factors.find(_.isInstanceOf[Constant[A]]).getOrElse(RationalNumber(1)).asInstanceOf[Constant[A]]
        val nonConst = factors.filterNot(_.isInstanceOf[Constant[A]])

        Expression.makeProduct(nonConst) -> const
      }
      case const: Constant[A] => (RationalNumber(1), const)
      case _ => this -> RationalNumber[A](1)
    }
  }

  protected def asFactor: (Expression[A], Expression[A]) = {
    // x -> (x, 1)
    // x*y -> (x*y, 1)
    // x^2 -> (x, 2)
    // x^y -> (x, y)
    // 2 -> (2, 1) /// TODO: consider rational numbers
    this match {
      case Power(base, exponent) => (base, exponent)
      case _ => (this, RationalNumber[A](1))
    }
  }
}

case class Sum[A](terms: Set[Expression[A]]) extends Expression[A]
case class Product[A](factors: Set[Expression[A]]) extends Expression[A]
case class Power[A](base: Expression[A], power: Expression[A]) extends Expression[A]
case class Variable[A](thing: A) extends Expression[A]

trait Constant[A] extends Expression[A]

case class RealNumber[A](value: Double) extends Constant[A]
case class RationalNumber[A](numerator: Int, denominator: Int = 1) extends Constant[A]

object Expression {
  val Zero = RationalNumber(0)

  def makeProduct[A](factors: Set[Expression[A]]): Expression[A] = {
    val nonOneFactors = factors.filterNot(_ == RationalNumber(1))
    nonOneFactors.size match {
      case 0 => RationalNumber(1)
      case 1 => nonOneFactors.head
      case _ => Product(nonOneFactors)
    }
  }

  def euclidsAlgorithm(x: Int, y: Int): Int = if (y == 0) x else euclidsAlgorithm(y, x % y)
}
