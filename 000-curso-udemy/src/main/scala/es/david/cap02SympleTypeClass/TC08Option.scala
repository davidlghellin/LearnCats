package es.david.cap02SympleTypeClass

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
