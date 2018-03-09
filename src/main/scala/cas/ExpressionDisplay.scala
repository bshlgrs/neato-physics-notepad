package cas

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
