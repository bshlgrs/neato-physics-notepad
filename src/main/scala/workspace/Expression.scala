package workspace

case class Expression[A](const: Double, factors: Map[A, Double]) {
  def simplifyWithEquivalenceClasses(equalities: SetOfSets[A]): Expression[A] =
    equalities.sets.foldLeft(this)({ case (expr: Expression[A], set: Set[A]) => expr.subMany(set, set.head)})

  def map[B](f: A => B) = Expression(const, factors.map({ case (k, v) => f(k) -> v }))

  def solve(x: A): Expression[A] = {
    val correctionPower = -1/factors(x)
    Expression(
      scala.math.pow(const, correctionPower),
      (factors - x).mapValues((power) => power * correctionPower)
    )
  }

  def sub(x: A, expr: Expression[A]): Expression[A] = {
    if (factors.contains(x)) {
      val exprWithoutX = this.copy(factors = factors - x)
      val xPower = factors(x)
      exprWithoutX * expr.exponentiate(xPower)
    } else this
  }

  def sub(x: A, y: A): Expression[A] = this.sub(x, Expression(1, Map(y -> 1)))

  def subMany(from: Set[A], to: A): Expression[A] =
    from.foldLeft(this)({ case (expr: Expression[A], a: A) => expr.sub(a, to)})

  def powerOf(x: A): Double = factors.getOrElse(x, 0)

  def *(that: Expression[A]): Expression[A] = {
    val allVariables = (this.factors.keys ++ that.factors.keys).toSet[A]
    val newFactors = allVariables.map((factor) => factor -> (this.powerOf(factor) + that.powerOf(factor))).toMap[A, Double]

    Expression[A](this.const * that.const, newFactors.filter(_._2 != 0))
  }

  def exponentiate(power: Double) = Expression(scala.math.pow(const, power), factors.mapValues(_ * power))

  def vars: Set[A] = factors.keySet
}

