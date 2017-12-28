package workspace

import scala.scalajs.js.annotation.JSExport

case class SetOfSets[A](sets: Set[Set[A]]) {
  def getSet(item: A): Set[A] = {
    sets.find(_.contains(item)).getOrElse(Set(item))
  }
  @JSExport
  def testEqual(x: A, y: A): Boolean = {
    this.getSet(x) == this.getSet(y)
  }

  def setEqual(x: A, y: A): SetOfSets[A] = SetOfSets(sets - getSet(x) - getSet(y) + (getSet(x) ++ getSet(y)))

  def remove(x: A): SetOfSets[A] = SetOfSets(sets.map(_ - x))
}
