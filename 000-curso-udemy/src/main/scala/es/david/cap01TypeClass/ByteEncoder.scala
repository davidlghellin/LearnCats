package es.david.cap01TypeClass

import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

trait Channel2 {
  def write[A](obj: A, enc: ByteEncoder[A]): Unit
}

object Ejer02TC extends Channel2 {
  override def write[A](obj: A, enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)

    Using(new FileOutputStream("000-curso-udemy/testEncoder.txt")) {
      os =>
        os.write(bytes)
        os.flush()
    }
  }

  def main(args: Array[String]): Unit = {
    // ahora necesitamos las instancias particulares
    object IntByteEncoder extends ByteEncoder[Int] {
      override def encode(a: Int): Array[Byte] = {
        val bb = ByteBuffer.allocate(4)
        bb.putInt(a)
        bb.array()
      }
    }

    object StrByteEncoder extends ByteEncoder[String] {
      override def encode(s: String): Array[Byte] = {
        s.getBytes
      }
    }

    Ejer02TC.write[Int](2, IntByteEncoder)
    Ejer02TC.write[String]("holaaa", StrByteEncoder)
  }
}