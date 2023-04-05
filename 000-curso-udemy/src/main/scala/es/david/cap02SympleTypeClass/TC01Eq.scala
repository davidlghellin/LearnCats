package es.david.cap02SympleTypeClass

import cats._
import cats.implicits._

case class Cuenta(id: Long, numero: String, saldo: Double, propietario: String)

object Cuenta {
  implicit val universalEq: Eq[Cuenta] = Eq.fromUniversalEquals // ==

  object Instances {
    implicit val byIdEq: Eq[Cuenta] = Eq.instance[Cuenta]((c1, c2) => Eq[Long].eqv(c1.id, c2.id))

    implicit def byIdEq2(implicit eqLong: Eq[Long]): Eq[Cuenta] = Eq.instance[Cuenta]((c1, c2) => eqLong.eqv(c1.id, c2.id))

    implicit def byIdEq3(implicit eqLong: Eq[Long]): Eq[Cuenta] = Eq.by(_.id)

    implicit def byNumeroEq(implicit eqString: Eq[String]): Eq[Cuenta] = Eq.by(_.numero)
  }
}

object TC01Eq extends App {

  val c1: Cuenta = Cuenta(1L, "", 1D, "")
  println(Eq[Cuenta].eqv(c1, c1))

  val c2: Cuenta = Cuenta(2L, "", 2D, "")
  println(Eq[Cuenta].eqv(c1, c2))

  println(Cuenta.Instances.byNumeroEq.eqv(c1, c2))

  // Cats syntax, por defecto el del companion object
  println(c1 === c2)

  // Alternativas para importar los implicitos de Instances
  //  import Cuenta.Instances.byNumeroEq
  implicit val eq: Eq[Cuenta] = Cuenta.Instances.byNumeroEq
  println(c1 === c2)
}
