package es.david.cap01TypeClass

import java.nio.ByteBuffer

trait ByteEncoderDerivation[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoderDerivation {
  def apply[A](implicit ev: ByteEncoderDerivation[A]): ByteEncoderDerivation[A] = ev
}

object Derivation extends App {

  implicit object StringByteEncoderDerivation extends ByteEncoderDerivation[String] {
    override def encode(a: String): Array[Byte] = {
      a.getBytes()
    }
  }

  implicit object IntByteEncoderDerivation extends ByteEncoderDerivation[Int] {
    override def encode(n: Int): Array[Byte] = {
      val bb = ByteBuffer.allocate(4)
      bb.putInt(n)
      bb.array()
    }
  }

  // esto sería lo que pensariamos hacer, pero sería un engorro y tendríamos que hacer las combinaciones
  //implicit object OptionString extends ByteEncoderDerivation[Option[String]] {
  //  override def encode(a: Option[String]): Array[Byte] = {
  //    a match {
  //      case Some(value) => StringByteEncoderDerivation.encode(value)
  //      case None => Array[Byte]()
  //    }
  //  }
  //}
  //
  //implicit object OptionInt extends ByteEncoderDerivation[Option[Int]] {
  //  override def encode(a: Option[Int]): Array[Byte] = {
  //    a match {
  //      case Some(value) => IntByteEncoderDerivation.encode(value)
  //      case None => Array[Byte]()
  //    }
  //  }
  //}

  implicit def optionEncoder[A](implicit encA: ByteEncoderDerivation[A]): ByteEncoderDerivation[Option[A]] = new ByteEncoderDerivation[Option[A]] {
    override def encode(a: Option[A]): Array[Byte] = {
      a match {
        case Some(value) => encA.encode(value)
        case None => Array[Byte]()
      }
    }
  }

  println(ByteEncoderDerivation[String].encode("hello").mkString(" "))
  println(ByteEncoderDerivation[Int].encode(1000).mkString(" "))
  println(ByteEncoderDerivation[Option[String]].encode(Option("world")).mkString(" "))
  println(ByteEncoderDerivation[Option[String]].encode(None).mkString(" "))
  println(ByteEncoderDerivation[Option[Int]].encode(Option(1000)).mkString(" "))
  println(ByteEncoderDerivation[Option[Int]].encode(None).mkString(" "))
}
