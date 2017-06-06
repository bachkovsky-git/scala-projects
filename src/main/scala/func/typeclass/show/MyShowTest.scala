package func.typeclass.show

import func.typeclass.show.MyShowSyntax._

object MyShowTest extends App {

  final case class Cat(name: String, age: Int, color: String)

  implicit object MyShowCat extends MyShow[Cat] {
    override def show(a: Cat): String =
      s"${a.name} is a ${a.age} year-old ${a.color} cat."
  }

  Cat("Giselle", 2, "gray") print

  case class Box[A](value: A)


  trait Printable[A] {
    self =>
    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] = b =>
      self.format(func(b))
  }

  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  implicit object StringPrintable extends Printable[String] {
    def format(value: String): String =
      "\"" + value + "\""
  }

  implicit object BooleanPrintable extends Printable[Boolean] {
    def format(value: Boolean): String =
      if (value) "yes" else "no"
  }

  format("hello")
  format(true)

  implicit def boxShow[A](implicit instance: Printable[A]): Printable[Box[A]] =
    instance.contramap[Box[A]](_.value)

  assert(format(Box("hello world")) == "\"hello world\"")
  assert(format(Box(true)) == "yes")

  trait Codec[A] {
    self =>
    def encode(value: A): String

    def decode(value: String): Option[A]

    def imap[B](dec: A => B, enc: B => A): Codec[B] =
      new Codec[B] {
        override def encode(value: B): String = self.encode(enc(value))

        override def decode(value: String): Option[B] = self.decode(value).map(dec)
      }
  }

  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): Option[A] =
    c.decode(value)

  implicit object IntCodec extends Codec[Int] {
    def encode(value: Int): String =
      value.toString

    def decode(value: String): Option[Int] =
      scala.util.Try(value.toInt).toOption
  }


  implicit def boxCodec[A](implicit aC: Codec[A]): Codec[Box[A]] =
    aC.imap[Box[A]](Box(_), _.value)

  assert(encode(Box(123)) == "123")
  assert(decode[Box[Int]]("123").contains(Box(123)))
}

