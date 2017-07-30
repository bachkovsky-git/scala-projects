import javax.swing.JButton

//this is ok in scala 2.12
val button = new JButton
button.addActionListener(_ => println("pressed!"))

def needStringArgument(s: String): String = s ++ s
implicit def intToStr(i: Int): String = i.toString

val ss = needStringArgument(111)

def printLength(seq: Seq[Int]) = println(seq.length)

implicit def intToRange(i: Int): Range.Inclusive = 1 to i

implicit def intToDigits(i: Int): List[Int] =
  i.toString.toList.map(_.toInt)

//ambiguous implicit conversion
//printLength(12)

val cba = "abc".reverse