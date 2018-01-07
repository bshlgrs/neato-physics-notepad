package workspace
import scala.annotation._
import scala.collection.immutable


object Util {
  def traceShow[A](a: A): A = {
    println(a)
    a
  }

  def showNumber(value: Double): String = {
//    if (value == 0) {
//      "0"
//    } else {
//      val exponent = math.log10(value).floor
//      if (exponent > 4) {
//        val significand = value / math.pow(10, exponent)
//        val significandDigits = (significand * math.pow(10, exponent)).round.toString.toList
//
//        "%.4g".format(value)
//        // Scientific notation. Round to 4 sig figs, eliding trailing zeros.
//      } else if (exponent > 0) {
//
//      }
//    }

    ???

    /*
    * Cases:
    *
    *
    * */
  }
}

case class MapWithIds[A](map: Map[Int, A], nextId: Int = 0) {
  type Pair = (Int, A)
  def get(key: Int): Option[A] = map.get(key)
  def apply(key: Int): A = map(key)
  def getOrElse(key: Int, other: A): A = map.getOrElse(key, other)
  def keySet: Set[Int] = map.keySet
  def keys: Iterable[Int] = map.keys
  def values: Iterable[A] = map.values
  def find(p: Pair => Boolean): Option[Pair] = map.find(p)
  def addWithNextId(thing: A): MapWithIds[A] = MapWithIds(map + (nextId -> thing), nextId + 1)
  def toSet: Set[Pair] = map.toSet
  def set(k: Int, v: A): MapWithIds[A] = this.copy(map + (k -> v))
  def delete(k: Int): MapWithIds[A] = this.copy(map - k)
  def mapValues[B](f: A => B): MapWithIds[B] = this.copy(map = map.mapValues(f))
}

object MapWithIds {
  def empty[A]: MapWithIds[A] = MapWithIds[A](Map(), 0)
}
