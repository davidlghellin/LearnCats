package es.david.cap02SympleTypeClass

import cats._
import cats.implicits._

case class CuentaShow(id: Long, numero: String, saldo: Double, propietario: String)

object CuentaShow {
  implicit val toStringShow: Show[CuentaShow] = Show.fromToString

  object Instances {
    implicit val byPropietariosYSaldo: Show[CuentaShow] = Show.show {
      cuenta =>
        s"${cuenta.propietario} - ${cuenta.saldo}"
    }
    implicit val byPropietario: Show[CuentaShow] = Show.show {
      cuenta =>
        s"La cuenta pertenece a ${cuenta.propietario}"
    }
  }
}

object TC03Show extends App {
  val c1: CuentaShow = CuentaShow(1L, "DavidCuetna", 123D, "David")
  println(CuentaShow.toStringShow.show(c1))
  println(CuentaShow.Instances.byPropietariosYSaldo.show(c1))
  println(CuentaShow.Instances.byPropietario.show(c1))
}
