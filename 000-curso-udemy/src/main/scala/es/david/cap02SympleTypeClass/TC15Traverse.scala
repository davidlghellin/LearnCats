package es.david.cap02SympleTypeClass

import cats._
import cats.implicits._

case class PersonaTraverse(nombre: String)


object TC15Traverse extends App {
  def findPersonaTraverseByNombre(name: String): Option[PersonaTraverse] = ???

  def findPersonasTraverseByNombres(nombres: MList[String]): Option[MList[PersonaTraverse]] = {
    nombres match {
      case MNil => Some(MNil)
      case MCons(h, t) => (findPersonaTraverseByNombre(h), findPersonasTraverseByNombres(t)).mapN(MCons.apply)
    }
  }

  def traverse[F[_] : Applicative, A, B](as: MList[A])(f: A => F[B]): F[MList[B]] = {
    as match {
      case MNil => Applicative[F].pure(MNil)
      case MCons(h, t) => (f(h), traverse(t)(f)).mapN(MCons.apply)
    }
  }

}
