package workspace
import scala.annotation._
import scala.collection.immutable


object Util {
  def traceShow[A](a: A): A = {
    println(a)
    a
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
