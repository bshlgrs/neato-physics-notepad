package cas

import workspace._
import workspace.SetOfSets
import workspace.dimensions._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel, ScalaJSDefined}

sealed trait Expression[A] {
  import ExpressionDisplay.wrap

  override def toString: String = this.toStringWithBinding._1

  def toStringWithBinding: (String, Int) = {
    this match {
      case Sum(list) => list.map((x) => wrap(x.toStringWithBinding, 0)).mkString(" + ") -> 0
      case Product(list) =>
        ExpressionDisplay.fractionDisplay(list, " ", (x: Expression[A]) => x.toStringWithBinding, (x) => s"√($x)", (n, d) => s"($n)/($d)") -> 1
      case Power(lhs, rhs) => wrap(lhs.toStringWithBinding, 2) + "<sup>" + wrap(rhs.toStringWithBinding, 0) + "</sup>" -> 0
      case Variable(thing) => thing.toString -> 3
      case RealNumber(r) => r.toString -> 3
      case RationalNumber(n, 1) => n.toString -> 3
      case RationalNumber(n, d) => s"$n/$d" -> 1
      case NamedNumber(_, name, _) => name -> 3
      case SpecialFunction(name, args) => s"$name(${args.map(_.toString).mkString(", ")})" -> 3
    }
  }

  def normalize: Expression[A] = this match {
    case Sum(list) => Sum(list.map(_.normalize).sortBy(_.hashCode()))
    case Product(list) => Product(list.map(_.normalize).sortBy(_.hashCode()))
    case _ => this
  }

  def equivalent(other: Expression[A]): Boolean = this.normalize == other.normalize

  def solve(x: A, lhs: Expression[A] = RationalNumber(0)): Set[Expression[A]] = {
    assert(!lhs.vars.contains(x))
    this match {
      case Sum(set) => {
        val (termsWithX, termsWithoutX) = set.partition(_.vars.contains(x))

        termsWithX.size match {
          // if x isn't here, we're hosed
          case 0 => Set()
          // if x is in exactly one of our terms (eg we know x*y + z == 0) then we solve for x in the term x is in
          case 1 => {
            val nonXTerms = lhs - Expression.makeSum(termsWithoutX)
            termsWithX.head.solve(x, nonXTerms)
          }
          case _ => {
            // This is situations like solving ax + bx == c for x -- you should do it by factoring.
            // TODO: implement that
            Set()
          }
        }
      }
      case Product(set) => {
        val (factorsWithX, factorsWithoutX) = set.partition(_.vars.contains(x))

        factorsWithX.size match {
          case 0 => Set()
          case 1 => {
            // This is like "solve a b^2 f(x) = y for x". The answer there is f^-1(y a^-1 b^-2)
            // which we get by doing (f(x)).solve(x, y a^-1 b^2)
            factorsWithX.head.solve(x, factorsWithoutX.map(_ ** RationalNumber[A](-1)).reduce(_ * _) * lhs)
          }
          case 2 => {
            // This equation might be quadratic, in which case you have a chance of solving it.
            Set()
          }
          case _ => Set() // this is hard
        }
      }
      case Variable(y) => if (x == y) Set(lhs) else Set()
      case _: Constant[A] => Set()
      case _: NamedNumber[A] => Set()
      case Power(base, power) => (base.vars.contains(x), power.vars.contains(x)) match {
        case (true, false) => // eg x^2
          Set(lhs ** (RationalNumber(1)/power))
        case (false, false) => Set()
        case (false, true) => ??? // We should be able to handle this with a logarithm
        case (true, true) => Set() // Transcendental equation, you're hosed
      }
      case SpecialFunction(_, _) => Set()
    }
  }

  def sqrt: Expression[A] = this ** RationalNumber[A](1, 2)

  def solve(x: Variable[A]): Set[Expression[A]] = this.solve(x.name)

  def subs(x: A): Expression[A] = ???

  def vars: Set[A] = this match {
    case Sum(terms) => terms.flatMap(_.vars).toSet
    case Product(factors) => factors.flatMap(_.vars).toSet
    case Power(lhs, rhs) => lhs.vars ++ rhs.vars
    case Variable(thing) => Set(thing)
    case _: Constant[_] => Set()
    case _: NamedNumber[_] => Set()
    case SpecialFunction(_, args) => args.flatMap(_.vars).toSet
  }

  def +(other: Expression[A]): Expression[A] = (this, other) match {
    case (RationalNumber(n1, d1), RationalNumber(n2, d2)) => {
      val numerator = n1 * d2 + n2 * d1
      val denominator = d1 * d2
      RationalNumber.build(numerator, denominator)
    }
    case (RealNumber(x), RationalNumber(n, d)) => RealNumber(x + n.toDouble / d)
    case (RationalNumber(n, d), RealNumber(x)) => RealNumber(x + n.toDouble / d)
    case (RealNumber(x), RealNumber(y)) => RealNumber(x + y)
    case (Sum(lhs), Sum(rhs)) => {
      val lhsTermTuples = lhs.map(_.asTerm).toMap
      val rhsTermTuples = rhs.map(_.asTerm).toMap

      val termTuples: Iterable[(Expression[A], Expression[A])] = (lhsTermTuples.keys ++ rhsTermTuples.keys).map((term: Expression[A]) =>
        term -> (lhsTermTuples.getOrElse(term, RationalNumber[A](0)) + rhsTermTuples.getOrElse(term, RationalNumber[A](0)))
      ).filter(_._2 != RationalNumber(0))

      // now we have a bunch of terms with the thing on the left and the coefficient on the right
      val terms = termTuples.map({ case (x, y) => x * y }).toList

      terms.size match {
        case 0 => RationalNumber(0)
        case 1 => terms.head
        case _ => Sum(terms)
      }
    }
    case (Sum(lhs), rhs) => {
      Sum(lhs) + Sum(List(rhs))
    }
    case (lhs, Sum(rhs)) => {
      Sum(List(lhs)) + Sum(rhs)
    }
    case _ => Sum(List(this)) + Sum(List(other))
  }

  def +(other: Int): Expression[A] = this + RationalNumber[A](other)

  def *(other: Expression[A]): Expression[A] = (this, other) match {
    case (RationalNumber(n1, d1), RationalNumber(n2, d2)) => {
      val numerator = n1 * n2
      val denominator = d1 * d2
      RationalNumber.build(numerator, denominator)
    }
    case (RealNumber(x), RationalNumber(n, d)) => RealNumber(x * n / d)
    case (RationalNumber(n, d), RealNumber(x)) => RealNumber(x * n / d)
    case (RealNumber(x), RealNumber(y)) => RealNumber(x * y)

    case (Product(lhs), Product(rhs)) => {
      val lhsFactorTuples = lhs.map(_.asFactor).toMap
      val rhsFactorTuples = rhs.map(_.asFactor).toMap

      val factorTuples: Iterable[(Expression[A], Expression[A])] = (lhsFactorTuples.keys ++ rhsFactorTuples.keys).map((factor: Expression[A]) =>
        factor -> (lhsFactorTuples.getOrElse(factor, RationalNumber[A](0)) + rhsFactorTuples.getOrElse(factor, RationalNumber[A](0)))
      ).filter(_._1 != RationalNumber(1))

      val factors = factorTuples.map({ case (x, y) => x ** y}).toList
      val (numericFactors, nonNumericFactors) = factors.partition(_.isInstanceOf[Constant[_]])
      val numericFactor = numericFactors.foldLeft(RationalNumber(1): Expression[A]) ({ case (acc, factor) => acc * factor })

      if (numericFactor == RationalNumber(0)) RationalNumber(0) else Expression.makeProduct(numericFactor +: nonNumericFactors)
    }
    case (Power(r1: RationalNumber[A], exp1), Power(r2: RationalNumber[A], exp2)) if exp1 == exp2 =>
      Expression.makePower[A](r1 * r2, exp1)
    case (_: Product[_], _) => this * Product(List(other))
    case (_, _: Product[_]) => Product(List(this)) * other
    case (_, _) => Product(List(this)) * Product(List(other))
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
    case Sum(terms) => terms.toList.map(_.mapVariablesToExpressions(f)).reduce(_ + _)
    case Product(factors) => {
      val mappedFactors = factors.toList.map(_.mapVariablesToExpressions(f))
      mappedFactors.reduce(_ * _)
    }
    case Variable(thing) => f(thing)
    case Power(lhs, rhs) => lhs.mapVariablesToExpressions(f) ** rhs.mapVariablesToExpressions(f)
    case RationalNumber(n, d) => RationalNumber(n, d)
    case RealNumber(x) => RealNumber(x)
    case NamedNumber(v, n, d) => NamedNumber(v, n, d)
    case SpecialFunction(name, args) => SpecialFunction(name, args.map(_.mapVariablesToExpressions(f)))
  }

  def mapVariables[B](f: A => B): Expression[B] = this.mapVariablesToExpressions((x) => Variable(f(x)))

  def substitute(from: A, to: A): Expression[A] = this.mapVariables((x) => if (x == from) to else x)
  def substitute(from: A, to: Expression[A]): Expression[A] =
    this.mapVariablesToExpressions((x) => if (x == from) to else Variable(x))

  def substituteMany(from: Set[A], to: A): Expression[A] = this.mapVariables((x) => if (from.contains(x)) to else x)

  def simplifyWithEquivalenceClasses(equalities: SetOfSets[A]): Expression[A] =
    equalities.sets.foldLeft(this)({ case (expr: Expression[A], set: Set[A]) => {
      expr.substituteMany(set, set.minBy(_.toString))
    }})

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
    case NamedNumber(x, _, _) => Some(x)
    case RationalNumber(n, d) => Some(n.toDouble / d)
    case SpecialFunction(name, args) => this match {
      case SpecialFunction("sin", List(x)) => x.evaluate.map(Math.sin)
      case SpecialFunction("cos", List(x)) => x.evaluate.map(Math.cos)
      case SpecialFunction("tan", List(x)) => x.evaluate.map(Math.tan)
      case SpecialFunction("asin", List(x)) => x.evaluate.map(Math.asin)
      case SpecialFunction("acos", List(x)) => x.evaluate.map(Math.acos)
      case SpecialFunction("atan", List(x)) => x.evaluate.map(Math.atan)
    }
  }

  def calculateDimension(getDimensionDirectly: A => DimensionInference): DimensionInference = this match {
    case Sum(terms) => {
      val calculatedDimensions: List[DimensionInference] = terms.toList.map(_.calculateDimension(getDimensionDirectly))

      calculatedDimensions.reduce((x: DimensionInference, y: DimensionInference) => {
        x.combineWithEquals(y)
      })
    }
    case Product(factors) => {
      val calculatedDimensions: List[DimensionInference] = factors.toList.map(_.calculateDimension(getDimensionDirectly))
      calculatedDimensions.reduce((x: DimensionInference, y: DimensionInference) => x.combine(y, _ * _))
    }
    case Power(base, exponent) => exponent.calculateDimension(getDimensionDirectly) match {
        // For consistency, the exponent has to be dimensionless, and either the base is dimensionless or the exponent is constant.
      case BottomDimensionInference => BottomDimensionInference
      case ConcreteDimensionInference(x) if x != SiDimension.Dimensionless => BottomDimensionInference
      case _ => {
        base.calculateDimension(getDimensionDirectly) match {
          case BottomDimensionInference => BottomDimensionInference
          case TopDimensionInference => TopDimensionInference
          case ConcreteDimensionInference(baseDimension) => {
            // If the baseDimension is not (), the exponent must be constant
            if (baseDimension == SiDimension.Dimensionless) {
              ConcreteDimensionInference(SiDimension.Dimensionless)
            } else {
              exponent.evaluate match {
                case None => BottomDimensionInference
                case Some(x) => {
                  ConcreteDimensionInference(baseDimension ** RationalNumber.makeFromDouble[String](x).get)
                }
              }
            }
          }
        }
      }
    }
    case Variable(name) => getDimensionDirectly(name)
    case _: Constant[_] => ConcreteDimensionInference(SiDimension.Dimensionless)
    case NamedNumber(v, n, dimension) => ConcreteDimensionInference(dimension)
    case SpecialFunction(name, List(angle)) => {
      if (Set("sin", "cos", "tan", "asin", "acos", "atan").contains(name)) {
        angle.calculateDimension(getDimensionDirectly) match {
          case BottomDimensionInference => BottomDimensionInference
          case TopDimensionInference => ConcreteDimensionInference(SiDimension.Dimensionless)
          case ConcreteDimensionInference(SiDimension.Dimensionless) => ConcreteDimensionInference(SiDimension.Dimensionless)
          case ConcreteDimensionInference(_) => BottomDimensionInference
        }
      } else {
        throw new NotImplementedError(s"Don't know how to infer dimensions on $this")
      }
    }
    case _ => throw new RuntimeException(s"I don't know how to do this asdfyuihk $this")
  }

  def differentiate(wrt: A): Expression[A] = this match {
    case Sum(terms) => terms.toList.map(_.differentiate(wrt)).reduce(_ + _)
    case Product(factors) => {
      val factorList = factors.toList
      factorList.zipWithIndex.map({ case (factor: Expression[A], idx: Int) =>
        (factorList.take(idx) ++ List(factor.differentiate(wrt)) ++ factorList.drop(idx + 1)).reduce(_ * _)}).reduce(_ + _)
    }
    case _: Constant[_] => RationalNumber(0)
    case _: NamedNumber[_] => RationalNumber(0)
    case Variable(x) => if (x == wrt) RationalNumber(1) else RationalNumber(0)
    case Power(base: Expression[A], exponent: Expression[A]) => {
      if (exponent.evaluate.isEmpty) {
        throw new RuntimeException("We don't have an implementation of differentiation for nonconstant exponents yet")
      } else {
        exponent * (base ** (exponent - 1)) * base.differentiate(wrt)
      }
    }
    case SpecialFunction("sin", List(angle)) => angle.differentiate(wrt) * SpecialFunction("cos", List(angle))
    case SpecialFunction("cos", List(angle)) => angle.differentiate(wrt) * SpecialFunction("sin", List(angle)) * -1
    case _ => {
      throw new NotImplementedError(s"don't know how to differentiate $this")
    }
  }

  def toJsObject: js.Object = this match {
    case Sum(terms) => js.Dynamic.literal("className" -> "Sum", "terms" -> js.Array(terms.map(_.toJsObject) :_*))
    case Product(factors) => js.Dynamic.literal("className" -> "Product", "factors" -> js.Array(factors.map(_.toJsObject) :_*))
    case RationalNumber(n, d) => js.Dynamic.literal("className" -> "RationalNumber", "numerator" -> n, "denominator" -> d)
    case RealNumber(r) => js.Dynamic.literal("className" -> "RealNumber", "value" -> r)
    case NamedNumber(value, name, dimension) => js.Dynamic.literal("className" -> "NamedNumber", "value" -> value, "name" -> name, "dimension" -> dimension.toJsObject)
    case Variable(x) => x match {
      case str: String => js.Dynamic.literal("className" -> "Variable", "name" -> str)
      case varId: VarId => js.Dynamic.literal("className" -> "Variable", "varId" -> varId.toJsObject)
      case _ => throw new RuntimeException(s"Tried to turn $this into JS object but cannot serialize")
    }
    case Power(base: Expression[A], exponent: Expression[A]) =>
      js.Dynamic.literal("className" -> "Power", "base" -> base.toJsObject, "exponent" -> exponent.toJsObject)
    case SpecialFunction(name, args) =>
      js.Dynamic.literal("className" -> "SpecialFunction", "name" -> name, "args" -> js.Array(args.map(_.toJsObject) :_*))
  }
}

//trait ExpressionJs extends



trait ExpressionJs extends js.Object {
  val className: String
  val terms: js.UndefOr[js.Array[ExpressionJs]]
  val factors: js.UndefOr[js.Array[ExpressionJs]]
  val args: js.UndefOr[js.Array[ExpressionJs]]
  val numerator: js.UndefOr[Int]
  val denominator: js.UndefOr[Int]
  val value: js.UndefOr[Double]
  val name: js.UndefOr[String]
  val dimension: js.UndefOr[SiDimensionJs]
  val varId: js.UndefOr[VarIdJs]
  val base: js.UndefOr[ExpressionJs]
  val exponent: js.UndefOr[ExpressionJs]
}

object ExpressionJs {
  def parseToType[A](expr: ExpressionJs, f: ExpressionJs => A): Expression[A] = expr.className match {
    case "Sum" => Sum(expr.terms.get.map(term => parseToType(term, f)).toList)
    case "Product" => Product(expr.factors.get.map(factor => parseToType(factor, f)).toList)
    case "RationalNumber" => RationalNumber(expr.numerator.get, expr.denominator.get)
    case "RealNumber" => RealNumber(expr.value.get)
    case "NamedNumber" => NamedNumber(expr.value.get, expr.name.get, SiDimensionJs.parse(expr.dimension.get))
    case "Variable" => Variable(f(expr))
    case "Power" => Power(parseToType(expr.base.get, f), parseToType(expr.exponent.get, f))
    case "SpecialFunction" => SpecialFunction(expr.name.get, expr.args.get.map(arg => parseToType(arg, f)).toList)
  }

  def parseToStringExpr(expr: ExpressionJs): Expression[String] = parseToType(expr, _.name.get)
  def parseToVarIdExpr(expr: ExpressionJs): Expression[VarId] = parseToType(expr, varExprJs => VarIdJs.parse(varExprJs.varId.get))
}

case class Sum[A](terms: List[Expression[A]]) extends Expression[A]
case class Product[A](factors: List[Expression[A]]) extends Expression[A]
case class Power[A](base: Expression[A], power: Expression[A]) extends Expression[A] {
  assert(power != RationalNumber(1), "power must not be 1")
}
case class Variable[A](name: A) extends Expression[A]

sealed trait Constant[A] extends Expression[A]

case class RealNumber[A](value: Double) extends Constant[A]
case class RationalNumber[A](numerator: Int, denominator: Int = 1) extends Constant[A] {
  assert(denominator > 0, "denominator must be > 0")

  def toDouble: Double = numerator.toDouble / denominator
}

trait RationalNumberJs extends js.Object {
  val numerator: Int
  val denominator: Int
}

case class NamedNumber[A](value: Double, name: String, dimension: SiDimension) extends Expression[A]

case class SpecialFunction[A](name: String, args: List[Expression[A]]) extends Expression[A]

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

  def makeFromDouble[A](double: Double): Option[RationalNumber[A]] = {
    (for {
      i <- Range(1, 10) // inclusive
      maybeNum = double * i
      if math.abs(maybeNum - maybeNum.round) < 0.0001
    } yield RationalNumber[A](maybeNum.round.toInt, i)).headOption
  }
}

object Expression {
  val Zero = RationalNumber(0)
  val Pi = NamedNumber(3.14159265359, "π", SiDimension.Dimensionless)

  def makeProduct[A](factors: List[Expression[A]]): Expression[A] = {
    val nonOneFactors = factors.filterNot(_ == RationalNumber(1))
    nonOneFactors.size match {
      case 0 => RationalNumber(1)
      case 1 => nonOneFactors.head
      case _ => Product(nonOneFactors)
    }
  }

  def makeSum[A](terms: List[Expression[A]]): Expression[A] = {
    val nonZeroTerms = terms.filterNot(_ == RationalNumber(0))
    nonZeroTerms.size match {
      case 0 => RationalNumber(0)
      case 1 => nonZeroTerms.head
      case _ => Sum(nonZeroTerms)
    }
  }

  def makePower[A](base: Expression[A], exponent: Expression[A]): Expression[A] = (base, exponent) match {
    case (_, RationalNumber(1, 1)) => base
    case (_, RationalNumber(0, 1)) => RationalNumber(1)
    case (RationalNumber(1, 1), _) => RationalNumber(1)
    case (RationalNumber(0, 1), _) => RationalNumber(0)
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
  def fractionDisplay[A](list: List[Expression[A]],
                         multiplySymbol: String,
                         toStringWithBinding: Expression[A] => (String, Int),
                         wrapWithSurd: String => String,
                         wrapWithFraction: (String, String) => String): String = {
    // TODO: Do something cleverer here: support grouped square roots and fractions
    val denominatorItems = list.collect({ case x@Power(_, RationalNumber(n, _)) if n < 0 => x })
    val numeratorItems = list.filterNot(denominatorItems.contains)

    val flippedDenominatorItems = denominatorItems.collect(
      { case Power(base, RationalNumber(n, d)) => Expression.makePower(base, RationalNumber(-n, d)): Expression[A] }
    )

    def groupWithRadical(items: List[Expression[A]]): String = {
      val itemsInsideRadical = items.collect({ case x@Power(_, RationalNumber(1, 2)) => x: Expression[A]})
      val outsideItems = items.filterNot(itemsInsideRadical.contains)

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

  def orderWithConstantsFirst[A](stuff: List[Expression[A]]): List[Expression[A]] = {
    val foo = (x: Expression[A]) => x match {
      case _: Constant[A] => 1
      case n: NamedNumber[A] => 2
      case v: Variable[A] => 3
      case _ => 3
    }

    stuff.sortBy(foo)
  }

  def wrap(tuple: (String, Int), binding: Int): String = if (tuple._2 >= binding) tuple._1 else s"(${tuple._1})"

  // todo: use these
  def unicodeForNumberSuperscript(int: Int): Char = "⁰¹²³⁴⁵⁶⁷⁸⁹".charAt(int)
  def unicodeForNumberSubscript(int: Int): Char = "₀₁₂₃₄₅₆₇₈₉".charAt(int)

  def makeIntSuperscripted(int: Int): String = int.toString.map(char => unicodeForNumberSuperscript("0123456789".indexOf(char)))
}
