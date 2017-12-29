package cas

import workspace.SetOfSets

trait Expression[A] {
  import ExpressionDisplay.wrap

  override def toString: String = this.toStringWithBinding._1

  def toStringWithBinding: (String, Int) = {
    this match {
      case Sum(set) => set.map((x) => wrap(x.toStringWithBinding, 0)).mkString(" + ") -> 0
      case Product(set) =>
        ExpressionDisplay.fractionDisplay(set, " ", (x: Expression[A]) => x.toStringWithBinding, (x) => s"√($x)", (n, d) => s"($n)/($d)") -> 1
      case Power(lhs, rhs) => wrap(lhs.toStringWithBinding, 2) + "<sup>" + wrap(rhs.toStringWithBinding, 0) + "</sup>" -> 0
      case Variable(thing) => thing.toString -> 3
      case RealNumber(r) => r.toString -> 3
      case RationalNumber(n, 1) => n.toString -> 3
      case RationalNumber(n, d) => s"$n/$d" -> 1
    }
  }

  def toLatex: String = this.toLatexWithBinding._1

  def toLatexWithBinding: (String, Int) = {
    this match {
      case Sum(set) => set.map((x) => wrap(x.toLatexWithBinding, 0)).mkString(" + ") -> 0
      case Product(set) =>
        ExpressionDisplay.fractionDisplay(set, " \\cdot ", (x: Expression[A]) => x.toStringWithBinding, (x) => s"\\sqrt{$x}", (n, d) => s"\\frac{$n}{$d}") -> 1
      case Power(lhs, rhs) => {
        rhs match {
          case RationalNumber(1, 2) => s"\\sqrt{${wrap(lhs.toLatexWithBinding, 0)}}" -> 3
          case RationalNumber(1, n) => s"\\sqrt[$n]{${wrap(lhs.toLatexWithBinding, 0)}}" -> 3
          case _ => (wrap(lhs.toLatexWithBinding, 2) + "^{" + wrap(rhs.toLatexWithBinding, 2) + "}") -> 2
        }
      }
      case Variable(thing) => thing.toString -> 3
      case RealNumber(r) => r.toString -> 3
      case RationalNumber(n, 1) => n.toString -> 3
      case RationalNumber(n, d) => s"\\frac{$n}{$d}" -> 3
    }
  }

  def solve(x: A, lhs: Expression[A] = RationalNumber(0)): List[Expression[A]] = {
    assert(!lhs.vars.contains(x))
    this match {
      case Sum(set) => {
        val (termsWithX, termsWithoutX) = set.partition(_.vars.contains(x))

        termsWithX.size match {
          // if x isn't here, we're hosed
          case 0 => List()
          // if x is in exactly one of our terms (eg we know x*y + z == 0) then we solve for x in the term x is in
          case 1 => {
            val nonXTerms = lhs - Expression.makeSum(termsWithoutX)
            termsWithX.head.solve(x, nonXTerms)
          }
          case _ => {
            // This is situations like solving ax + bx == c for x -- you should do it by factoring.
            // TODO: implement that
            List()
          }
        }
      }
      case Product(set) => {
        val (factorsWithX, factorsWithoutX) = set.partition(_.vars.contains(x))

        factorsWithX.size match {
          case 0 => List()
          case 1 => {
            // This is like "solve a b^2 f(x) = y for x". The answer there is f^-1(y a^-1 b^-2)
            // which we get by doing (f(x)).solve(x, y a^-1 b^2)
            factorsWithX.head.solve(x, factorsWithoutX.map(_ ** RationalNumber[A](-1)).reduce(_ * _) * lhs)
          }
          case 2 => {
            // This equation might be quadratic, in which case you have a chance of solving it.
            List()
          }
          case _ => List() // this is hard
        }
      }
      case Variable(y) => if (x == y) List(lhs) else List()
      case _: Constant[A] => List()
      case Power(base, power) => (base.vars.contains(x), power.vars.contains(x)) match {
        case (true, false) => // eg x^2
          List(lhs ** (RationalNumber(1)/power))
        case (false, false) => List()
        case (false, true) => ??? // We should be able to handle this with a logarithm
        case (true, true) => List() // Transcendental equation, you're hosed
      }
    }
  }

  def sqrt: Expression[A] = this ** RationalNumber[A](1, 2)

  def solve(x: Variable[A]): List[Expression[A]] = this.solve(x.thing)

  def subs(x: A): Expression[A] = ???

  def vars: Set[A] = this match {
    case Sum(terms) => terms.flatMap(_.vars)
    case Product(factors) => factors.flatMap(_.vars)
    case Power(lhs, rhs) => lhs.vars ++ rhs.vars
    case Variable(thing) => Set(thing)
    case _: Constant[_] => Set()
  }

  def +(other: Expression[A]): Expression[A] = (this, other) match {
    case (RationalNumber(n1, d1), RationalNumber(n2, d2)) => {
      val numerator = n1 * d2 + n2 * d1
      val denominator = d1 * d2
      RationalNumber.build(numerator, denominator)
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

  def +(other: Int): Expression[A] = this + RationalNumber[A](other)

  def *(other: Expression[A]): Expression[A] = (this, other) match {
    case (RationalNumber(n1, d1), RationalNumber(n2, d2)) => {
      val numerator = n1 * n2
      val denominator = d1 * d2
      RationalNumber.build(numerator, denominator)
    }
    case (Product(lhs), Product(rhs)) => {
      val lhsFactorTuples = lhs.map(_.asFactor).toMap
      val rhsFactorTuples = rhs.map(_.asFactor).toMap

      val factorTuples: Iterable[(Expression[A], Expression[A])] = (lhsFactorTuples.keys ++ rhsFactorTuples.keys).map((factor: Expression[A]) =>
        factor -> (lhsFactorTuples.getOrElse(factor, RationalNumber[A](0)) + rhsFactorTuples.getOrElse(factor, RationalNumber[A](0)))
      ).filter(_._1 != RationalNumber(1))

      val factors = factorTuples.map({ case (x, y) => x ** y}).toSet
      val (numericFactors, nonNumericFactors) = factors.partition(_.isInstanceOf[Constant[_]])
      val numericFactor = numericFactors.foldLeft(RationalNumber(1): Expression[A]) ({ case (acc, factor) => acc * factor })

      if (numericFactor == RationalNumber(0)) RationalNumber(0) else Expression.makeProduct(nonNumericFactors + numericFactor)
    }
    case (_: Product[_], _) => this * Product(Set(other))
    case (_, _: Product[_]) => Product(Set(this)) * other
    case (_, _) => Product(Set(this)) * Product(Set(other))
  }
  def *(other: Int): Expression[A] = this * RationalNumber[A](other)

  def -(other: Expression[A]): Expression[A] = this + RationalNumber(-1) * other
  def -(other: Int): Expression[A] = this - RationalNumber[A](other)

  def /(other: Expression[A]): Expression[A] = this * (other ** RationalNumber[A](-1))
  def /(other: Int): Expression[A] = this / RationalNumber[A](other)

  def **(other: Expression[A]): Expression[A] = (this, other) match {
    case (RationalNumber(1, 1), _) => RationalNumber(1)
    case (RationalNumber(0, 1), _) => RationalNumber(0)
    case (RationalNumber(a, b), RationalNumber(c, d)) => {
      if(c < 0) {
        if (a > 0)
          RationalNumber[A](b, a) ** RationalNumber[A](-c, d)
        else
          RationalNumber[A](-b, -a) ** RationalNumber[A](-c, d)
      } else {
        if (d == 1) {
          val (num, den) = (scala.math.pow(a, c).toInt, scala.math.pow(b, c).toInt)
          RationalNumber.build(num, den)
        } else {
          // todo: this can be somewhat simplified sometimes. Eg sqrt(3/4) => sqrt(3)/2
          Power(this, other)
        }
      }
    }
    case (_, RationalNumber(1, 1)) => this
    case (_, RationalNumber(0, 1)) => RationalNumber(1)
    case (Product(factors), exponent) => factors.map(_ ** exponent).reduce(_ * _)
    case (Power(base, exponent), newExponent) => base ** (exponent * newExponent)
    case _ => Power(this, other)
  }
  def **(other: Int): Expression[A] = this ** RationalNumber[A](other)

  def mapVariablesToExpressions[B](f: A => Expression[B]): Expression[B] = this match {
    case Sum(terms) => terms.map(_.mapVariablesToExpressions(f)).reduce(_ + _)
    case Product(factors) => {
      val mappedFactors = factors.map(_.mapVariablesToExpressions(f))
      mappedFactors.reduce(_ * _)
    }
    case Variable(thing) => f(thing)
    case Power(lhs, rhs) => lhs.mapVariablesToExpressions(f) ** rhs.mapVariablesToExpressions(f)
    case RationalNumber(n, d) => RationalNumber(n, d)
    case RealNumber(x) => RealNumber(x)
  }

  def mapVariables[B](f: A => B): Expression[B] = this.mapVariablesToExpressions((x) => Variable(f(x)))

  def substitute(from: A, to: A): Expression[A] = this.mapVariables((x) => if (x == from) to else x)
  def substitute(from: A, to: Expression[A]): Expression[A] =
    this.mapVariablesToExpressions((x) => if (x == from) to else Variable(x))

  def substituteMany(from: Set[A], to: A): Expression[A] = this.mapVariables((x) => if (from.contains(x)) to else x)

  def simplifyWithEquivalenceClasses(equalities: SetOfSets[A]): Expression[A] =
    equalities.sets.foldLeft(this)({ case (expr: Expression[A], set: Set[A]) => expr.substituteMany(set, set.minBy(_.toString))})

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

  def evaluate: Option[Double] = this match {
    case Sum(terms) => {
      val termValues = terms.map(_.evaluate)
      if (termValues.forall(_.isDefined)) {
        Some(termValues.map(_.get).sum)
      } else None
    }
    case Product(factors) => {
      val factorsValues = factors.map(_.evaluate)
      if (factorsValues.forall(_.isDefined)) {
        Some(factorsValues.map(_.get).product)
      } else None
    }
    case Power(lhs, rhs) => (lhs.evaluate, rhs.evaluate) match {
      case (Some(lhsVal), Some(rhsVal)) => Some(math.pow(lhsVal, rhsVal))
      case _ => None
    }
    case _: Variable[_] => None
    case RealNumber(x) => Some(x)
    case NamedNumber(x, _) => Some(x)
    case RationalNumber(n, d) => Some(n.toDouble / d)
  }
}

case class Sum[A](terms: Set[Expression[A]]) extends Expression[A]
case class Product[A](factors: Set[Expression[A]]) extends Expression[A]
case class Power[A](base: Expression[A], power: Expression[A]) extends Expression[A] {
  assert(power != RationalNumber(1), "power must not be 1")
}
case class Variable[A](thing: A) extends Expression[A]

trait Constant[A] extends Expression[A]

case class RealNumber[A](value: Double) extends Constant[A]
case class RationalNumber[A](numerator: Int, denominator: Int = 1) extends Constant[A] {
  assert(denominator > 0, "denominator must be > 0")
}
case class NamedNumber[A](value: Double, name: String) extends Constant[A]

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
      if(denominator <= 0) {
        println("oh dear")
      }
      RationalNumber(numerator / gcd, denominator / gcd)
    }
  }
}

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

  def makeSum[A](terms: Set[Expression[A]]): Expression[A] = {
    val nonZeroTerms = terms.filterNot(_ == RationalNumber(0))
    nonZeroTerms.size match {
      case 0 => RationalNumber(0)
      case 1 => nonZeroTerms.head
      case _ => Sum(nonZeroTerms)
    }
  }

  def makePower[A](base: Expression[A], exponent: Expression[A]): Expression[A] = exponent match {
    case RationalNumber(1, 1) => base
    case _ => Power(base, exponent)
  }

  def euclidsAlgorithm(x: Int, y: Int): Int = if (y == 0) x else euclidsAlgorithm(y, x % y)

  def buildGoofily(number: Constant[String], factors: Map[String, Int]): Expression[String] = {
    // we get in a bunch of factors. Multiply them all together and say they're equal to 1
    (number * factors.map({case (name, power) => Expression.makePower(Variable(name), RationalNumber(power)) : Expression[String]}).reduce(_ * _)
      - 1)
  }

  def buildGoofily(factors: Map[String, Int]): Expression[String] = buildGoofily(RationalNumber(1), factors)
}

object ExpressionDisplay {
  def fractionDisplay[A](set: Set[Expression[A]],
                         multiplySymbol: String,
                         toStringWithBinding: Expression[A] => (String, Int),
                         wrapWithSurd: String => String,
                         wrapWithFraction: (String, String) => String): String = {
    // TODO: Do something cleverer here: support grouped square roots and fractions
    val denominatorItems = set.collect({ case x@Power(_, RationalNumber(n, _)) if n < 0 => x })
    val numeratorItems = set -- denominatorItems

    val flippedDenominatorItems = denominatorItems.collect(
      { case Power(base, RationalNumber(n, d)) => Expression.makePower(base, RationalNumber(-n, d)): Expression[A] }
    )

    def groupWithRadical(items: Set[Expression[A]]): String = {
      val itemsInsideRadical = items.collect({ case x@Power(_, RationalNumber(1, 2)) => x: Expression[A]})
      val outsideItems = items -- itemsInsideRadical

      val radicalStr = if (itemsInsideRadical.nonEmpty) {
        val insideItemStrings = orderWithConstantsFirst(itemsInsideRadical)
          .collect({ case Power(base, _) => wrap(toStringWithBinding(base), 1)})

        wrapWithSurd(insideItemStrings.mkString(multiplySymbol))
      } else ""

      s"${orderWithConstantsFirst(outsideItems).map((x) => wrap(toStringWithBinding(x), 1)).mkString(multiplySymbol)} $radicalStr"
    }

    if (denominatorItems.nonEmpty) {
      if (numeratorItems.isEmpty) {
        wrapWithFraction("1", groupWithRadical(flippedDenominatorItems))
      } else {
        wrapWithFraction(groupWithRadical(numeratorItems), groupWithRadical(flippedDenominatorItems))
      }
    } else {
      groupWithRadical(numeratorItems)
    }
  }

  def orderWithConstantsFirst[A](stuff: Set[Expression[A]]): List[Expression[A]] = {
    val (first, second) = stuff.partition(_.isInstanceOf[Constant[_]])
    first.toList ++ second.toList
  }

  def wrap(tuple: (String, Int), binding: Int): String = if (tuple._2 >= binding) tuple._1 else s"(${tuple._1})"

  def main(args: Array[String]): Unit = {
    println(orderWithConstantsFirst(Set[Expression[String]](Variable("x"), Variable("y"), RationalNumber[String](2))))
  }
}
