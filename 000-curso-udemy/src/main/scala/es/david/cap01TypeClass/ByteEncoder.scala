package es.david.cap01TypeClass

import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

trait ByteEncoder[A] {
  def encode(a: A): Array[Byte]
}

object ByteEncoder {
  implicit object IntByteEncoder extends ByteEncoder[Int] {
    override def encode(a: Int): Array[Byte] = {
      val bb = ByteBuffer.allocate(4)
      bb.putInt(a)
      bb.array()
    }
  }

  implicit object StrByteEncoder extends ByteEncoder[String] {
    override def encode(s: String): Array[Byte] = {
      s.getBytes
    }
  }
}

trait Channel2 {
  def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit
}

object Ejer02TC extends Channel2 {
  override def write[A](obj: A)(implicit enc: ByteEncoder[A]): Unit = {
    val bytes: Array[Byte] = enc.encode(obj)

    Using(new FileOutputStream("000-curso-udemy/testEncoder.txt")) {
      os =>
        os.write(bytes)
        os.flush()
    }
  }

  def main(args: Array[String]): Unit = {
    // ahora necesitamos las instancias particulares

    Ejer02TC.write[Int](2)
    Ejer02TC.write[String]("holaaa")

    // Si queremos uno particular podemos pasar, explicitamente
    object Mas3StrByteEncoder extends ByteEncoder[String] {
      override def encode(s: String): Array[Byte] = {
        s.getBytes.map(b => (b + 3).toByte)
      }
    }

    Ejer02TC.write[String]("holaaa")(Mas3StrByteEncoder)

    import Switch._
    Ejer02TC.write[Switch](Switch(true))
  }
}

case class Switch(isOn: Boolean)
object Switch {
  implicit object SwitchByteEncoder extends ByteEncoder[Switch] {
    override def encode(a: Switch): Array[Byte] = {
      Array(
        a.isOn match {
          case true => 1.toByte
          case false => 0.toByte
        }
      )
    }
  }
}
