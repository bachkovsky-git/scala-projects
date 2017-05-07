package func

import func.FMap.fromList

trait FMap[K, V] {
  def apply(key: K): Option[V] = get(key)

  def get(key: K): Option[V]

  def put(key: K, value: V): FMap[K, V]

  def delete(key: K): FMap[K, V]
}

object FMap {
  def empty[K, V](): FMap[K, V] = FuncMap(_ => None)

  def fromList[K, V](list: List[(K, V)]): FMap[K, V] = list match {
    case Nil => empty()
    case (k, v) :: tail => fromList(tail).put(k, v)
  }
}

case class FuncMap[K, V](f: K => Option[V]) extends FMap[K, V] {
  override def get(x: K): Option[V] = f(x)

  override def put(key: K, value: V): FMap[K, V] = FuncMap {
    case `key` => Some(value)
    case x     => f(x)
  }

  override def delete(key: K): FMap[K, V] = FuncMap {
    case `key` => None
    case x     => f(x)
  }
}

object Test extends App {
  val map = fromList((1, "A") :: (3, "B") :: (5, "C") :: Nil)

  val map1 = map.put(1, "KEK").put(2, "SOS").delete(3)

  assert(map(1).contains("A"))
  assert(map1(1).contains("KEK"))
  assert(map1(3).isEmpty)
}