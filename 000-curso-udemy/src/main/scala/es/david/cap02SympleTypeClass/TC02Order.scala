package es.david.cap02SympleTypeClass

import cats._
import cats.implicits._

case class CuentaOrder(id: Long, numero: String, saldo: Double, propietario: String)

object CuentaOrder {
  implicit def orderById(implicit orderLong: Order[Long]): Order[CuentaOrder] = Order.from((c1, c2) => orderLong.compare(c1.id, c2.id))

  object Instances {
    implicit val orderByNumber: Order[CuentaOrder] = Order.by(cuenta => cuenta.numero)

    implicit def orderBySaldo(implicit orderDouble: Order[Double]): Order[CuentaOrder] = Order.by(cuenta => cuenta.saldo)
  }
}

object TC02Order extends App {
  val c1: CuentaOrder = CuentaOrder(1L, "", 1D, "")
  println(Order[CuentaOrder].eqv(c1, c1))

  val c2: CuentaOrder = CuentaOrder(2L, "", 2D, "")
  println(Order[CuentaOrder].eqv(c1, c2))


  def sort[A](list: List[A])(implicit orderA: Order[A]) = {
    list.sorted(orderA.toOrdering)
  }

  val c3: CuentaOrder = CuentaOrder(0L, "", 2D, "")
  val lista = List(c1, c1, c2, c3)
  println(sort[CuentaOrder](lista))

  println(c1 compare c2)
  // Result of comparing x with y. Returns an Int whose sign is: - negative iff x < y - zero iff x = y - positive iff

  println(c1 min c2)
  println(c1 max c2)
}
