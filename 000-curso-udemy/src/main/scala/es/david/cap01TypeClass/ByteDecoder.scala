package es.david.cap01TypeClass

import scala.util.Try

trait ByteDecoder[A] {
  def decode(bytes: Array[Byte]): Option[A]
}

object ByteDecoder {
  // 1º añadir apply e instance para ByteDecoder
  def apply[A](implicit ev: ByteDecoder[A]): ByteDecoder[A] = ev

  def instance[A](f: Array[Byte] => Option[A]): ByteDecoder[A] = new ByteDecoder[A] {
    override def decode(bytes: Array[Byte]): Option[A] = f(bytes)
  }
}


object Main extends App {
  // 2º escribir una instancia de ByteDecoder[String]
  implicit object StringByteDecoder extends ByteDecoder[String] {
    override def decode(bytes: Array[Byte]): Option[String] = {
      Try(new String(bytes)).toOption

    }
  }

  // usa la instancia implicita
  val array1: Array[Byte] = Array(98, 105, 101, 101, 32, 58, 41)

  println(StringByteDecoder.decode(array1))

  println(ByteDecoder[String].decode(array1))
}