package es.david.cap01TypeClass

import java.io.FileOutputStream
import scala.util.Using

trait ByteEncodable {
  def encode(): Array[Byte]
}

trait Channel1 {
  def write(obj: ByteEncodable): Unit
}

case class FullName1(nombre: String, apellido: String) extends ByteEncodable {
  override def encode(): Array[Byte] = {
    nombre.getBytes ++ apellido.getBytes
  }
}

object FileChanel1 extends Channel1 {
  override def write(obj: ByteEncodable): Unit = {
    val bytes: Array[Byte] = obj.encode()

    Using(new FileOutputStream("000-curso-udemy/testFull.txt")) {
      os =>
        os.write(bytes)
        os.flush()
    }
  }

  def main(args: Array[String]): Unit = {
    FileChanel1.write(FullName1("David", "Lopez"))
  }
}
