package es.david.cap01TypeClass

import java.nio.ByteBuffer

object Syntax extends App {
  trait ByteEncoderSyntax[A] {
    def encode(a: A): Array[Byte]
  }

  implicit object IntByteEncoderSyntax extends ByteEncoderSyntax[Int] {
    override def encode(n: Int): Array[Byte] = {
      val bb = ByteBuffer.allocate(4)
      bb.putInt(n)
      bb.array()
    }
  }

  implicit object StringByteEncoderSyntax extends ByteEncoderSyntax[String] {
    override def encode(a: String): Array[Byte] = {
      a.getBytes
    }
  }

  // Syntax
  implicit class ByteEncoderSyntaxOps[A](val a: A) extends AnyVal {
    def encode(implicit enc: ByteEncoderSyntax[A]): Array[Byte] =
      enc.encode(a)
  }

  println(5.encode.mkString(" "))

  println("hello world".encode.mkString(" "))
  println(new ByteEncoderSyntaxOps[String]("hello world").encode.mkString(" "))
  println(StringByteEncoderSyntax.encode("hello world").mkString(" "))

  trait ByteDecoder[A] {
    def decode(bytes: Array[Byte]): Option[A]
  }

  implicit object IntByteDecoder extends ByteDecoder[Int] {
    override def decode(bytes: Array[Byte]): Option[Int] = {
      if(bytes.length != 4) None
      else {
        val bb = ByteBuffer.allocate(4)
        bb.put(bytes)
        bb.flip()
        Some(bb.getInt())
      }
    }
  }

  implicit class ByteDecoderOps[A](bytes: Array[Byte]) {
    def decode(implicit dec: ByteDecoder[A]): Option[A] = {
      dec.decode(bytes)
    }
  }

  println(Array[Byte](0, 0, 0, 5).decode)
  println(Array[Byte](0, 0, 0, 0, 5).decode)
}
