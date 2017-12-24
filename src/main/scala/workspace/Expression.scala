package workspace

case class Expression2[A](const: Double, factors: Map[A, Double]) {
  def simplifyWithEquivalenceClasses(equalities: SetOfSets[A]): Expression2[A] =
    equalities.sets.foldLeft(this)({ case (expr: Expression2[A], set: Set[A]) => expr.substituteMany(set, set.head)})

  def map[B](f: A => B) = Expression2(const, factors.map({ case (k, v) => f(k) -> v }))

  def solve(x: A): Expression2[A] = {
    val correctionPower = -1/factors(x)
    Expression2(
      scala.math.pow(const, correctionPower),
      (factors - x).mapValues((power) => power * correctionPower)
    )
  }

  def substitute(x: A, expr: Expression2[A]): Expression2[A] = {
    if (factors.contains(x)) {
      val exprWithoutX = this.copy(factors = factors - x)
      val xPower = factors(x)
      exprWithoutX * expr.exponentiate(xPower)
    } else this
  }

  def substitute(x: A, y: A): Expression2[A] = this.substitute(x, Expression2(1, Map(y -> 1)))

  def substituteMany(from: Set[A], to: A): Expression2[A] =
    from.foldLeft(this)({ case (expr: Expression2[A], a: A) => expr.substitute(a, to)})

  def powerOf(x: A): Double = factors.getOrElse(x, 0)

  def *(that: Expression2[A]): Expression2[A] = {
    val allVariables = (this.factors.keys ++ that.factors.keys).toSet[A]
    val newFactors = allVariables.map((factor) => factor -> (this.powerOf(factor) + that.powerOf(factor))).toMap[A, Double]

    Expression2[A](this.const * that.const, newFactors.filter(_._2 != 0))
  }

  def exponentiate(power: Double) = Expression2(scala.math.pow(const, power), factors.mapValues(_ * power))

  def vars: Set[A] = factors.keySet
}

