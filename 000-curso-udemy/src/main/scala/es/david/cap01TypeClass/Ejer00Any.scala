package es.david.cap01TypeClass

import java.io.FileOutputStream
import java.nio.ByteBuffer
import scala.util.Using

trait Channel {
  def write(obj: Any): Unit
}

object Ejer00Any extends Channel {
  override def write(obj: Any): Unit = {
    val bytes: Array[Byte] = obj match {
      case n: Int =>
        val bb = ByteBuffer.allocate(4)
        bb.putInt(n)
        bb.array()

      case s: String => s.getBytes
      //      s.map(c => c.toByte).toArray
      case _ => throw new Exception("no definido")
    }

    Using(new FileOutputStream("000-curso-udemy/test.txt")) {
      os =>
        os.write(bytes)
        os.flush()
    }
  }

  def main(args: Array[String]): Unit = {
    Ejer00Any.write("hola")
  }
}
