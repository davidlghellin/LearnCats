package es.david.cap02SympleTypeClass

import cats.Monad
import cats.implicits.catsStdInstancesForOption

sealed trait MOption[+A] {
  def flatMap[A, B](fa: MOption[A])(f: A => MOption[B]): MOption[B] =
    fa match {
      case MSome(a) => f(a)
      case MNone => MNone
    }
}

case class MSome[+A](a: A) extends MOption[A]

case object MNone extends MOption[Nothing]

object TC08Option extends App {

  case class PersonaOpt(name: String)

  case class Cuenta(balance: Double, owner: PersonaOpt)

  case class Transferencia(origen: Cuenta, destino: Cuenta, cantidad: Double)

  def findPersonByNameMOption(name: String): MOption[PersonaOpt] = ???

  def findCuentaByPersonaMOption(persona: PersonaOpt): MOption[Cuenta] = ???

  def findLastTransferByOrigenDeLaCuentaMOption(cuenta: Cuenta): MOption[Transferencia] = ???

  // podriamos tener algo así, pero lia mucho, para eso tenemos el flatmap
  def findLastTransferByNombrePersonaMOption(name: String): MOption[Transferencia] = {
    findPersonByNameMOption(name) match {
      case MSome(persona) => findCuentaByPersonaMOption(persona) match {
        case MSome(acc) => findLastTransferByOrigenDeLaCuentaMOption(acc)
        case MNone => MNone
      }
      case MNone => MNone
    }
  }

  // Lo replicamos algo tengo mal en el flatmap, usamos el Option normal

  def findPersonByName(name: String): Option[PersonaOpt] = ???

  def findCuentaByPersona(persona: PersonaOpt): Option[Cuenta] = ???

  def findLastTransferByOrigenDeLaCuenta(cuenta: Cuenta): Option[Transferencia] = ???

  def findLastTransferByNombrePersonaFlatMap(name: String): Option[Transferencia] =
    findPersonByName(name).flatMap { persona =>
      findCuentaByPersona(persona).flatMap { acc =>
        findLastTransferByOrigenDeLaCuenta(acc)
      }
    }

  val forExpresionFindLastTransferByNombrePersona: Option[Transferencia] = for {
    persona <- findPersonByName("name")
    acc <- findCuentaByPersona(persona)
    transfer <- findLastTransferByOrigenDeLaCuenta(acc)
  } yield transfer

}

object Ejercicio extends App {
  sealed trait MOptionEjer[+A]

  case class MSomeEjer[+A](a: A) extends MOptionEjer[A]

  case object MNoneEjer extends MOptionEjer[Nothing]

  implicit val monadOptionEjer: Monad[MOptionEjer] = new Monad[MOptionEjer] {
    override def pure[A](x: A): MOptionEjer[A] = MSomeEjer(x)

    override def flatMap[A, B](fa: MOptionEjer[A])(f: A => MOptionEjer[B]): MOptionEjer[B] = fa match {
      case MSomeEjer(a) => f(a)
      case MNoneEjer => MNoneEjer
    }

    override def map[A, B](fa: MOptionEjer[A])(f: A => B): MOptionEjer[B] =
      flatMap(fa)(a => pure(f(a)))

    override def flatten[A](ffa: MOptionEjer[MOptionEjer[A]]): MOptionEjer[A] =
      flatMap(ffa)(x => x)

    override def tailRecM[A, B](a: A)(f: A => MOptionEjer[Either[A, B]]): MOptionEjer[B] = ???

  }
  val opt: Option[Int] = Some(2).flatMap(i => Some(i + 2))
  println(opt)

  val optOpt: Option[Option[Int]] = Monad[Option].pure(Monad[Option].pure(54))
  println(optOpt)
  println(optOpt.flatten)
  println(optOpt.flatten)
}
