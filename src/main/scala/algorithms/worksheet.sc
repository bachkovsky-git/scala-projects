def insertSort[T: Ordering](arr: Array[T]): Array[T] = {
  import scala.math.Ordering.Implicits._

  val res = arr.clone //create copy
  for (j <- 1 until res.length) {
    val key = res(j)
    var i = j - 1
    while (i >= 0 && res(i) > key) {
      res(i + 1) = res(i)
      i -= 1
    }
    res(i + 1) = key
  }
  res
}

def test(sort: Array[Int] => Array[Int]): Unit = {
  import scala.util.Random

  def testOn(data: Array[Int]): Unit = assert(sort(data) sameElements data.sorted)

  val randomIntegers = Stream.continually(Random.nextInt)

  testOn(Array.empty[Int])
  testOn(Array(1))
  testOn(randomIntegers.take(1000).toArray)
}

test(insertSort)