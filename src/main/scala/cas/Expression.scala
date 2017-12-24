package cas

trait Expression[A] {
  import Expression._
  def solve(x: A): List[Expression[A]] = {
    ???
  }

  def subs(x: A): Expression[A] = ???

  def +(other: Expression[A]): Expression[A] = (this, other) match {
    case (RationalNumber(n1, d1), RationalNumber(n2, d2)) => {
      assert(d1 == d2) // todo: generalize
      RationalNumber(n1 + n2, d1)
    }
    case (Add(lhs), Add(rhs)) => {
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
        case _ => Add(terms)
      }
    }
    case (Add(lhs), rhs) => {
      Add(lhs) + Add(Set(rhs))
    }
    case (lhs, Add(rhs)) => {
      Add(Set(lhs)) + Add(rhs)
    }
    case _ => Add(Set(this)) + Add(Set(other))
  }

  def *(other: Expression[A]): Expression[A] = (this, other) match {
    case (RationalNumber(n1, d1), RationalNumber(n2, d2)) => {
      // todo: reduce this fraction
      RationalNumber[A](n1 * n2, d1 * d2)
    }
    case (Product(lhs), Product(rhs)) => {
      // TODO: I don't know why this needs so many type annotations
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

  def /(other: Expression[A]): Expression[A] = this * other ** RationalNumber(-1)

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

case class Add[A](terms: Set[Expression[A]]) extends Expression[A]
case class Product[A](factors: Set[Expression[A]]) extends Expression[A]
case class Power[A](base: Expression[A], power: Expression[A]) extends Expression[A]
case class Variable[A](thing: A) extends Expression[A]

trait Constant[A] extends Expression[A]

case class RealNumber[A](value: Double) extends Constant[A]
case class RationalNumber[A](numerator: Int, denominator: Int = 1) extends Constant[A]

object Expression {
  val Zero = RationalNumber(0)

  def makeProduct[A](exprs: Set[Expression[A]]): Expression[A] = exprs.size match {
    case 0 => RationalNumber(1)
    case 1 => exprs.head
    case _ => Product(exprs)
  }
}
