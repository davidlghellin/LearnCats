package es.david.cap02SympleTypeClass

import cats.Functor

import java.nio.charset.StandardCharsets
import java.security.MessageDigest

case class PersonaF(nombre: String)

case class PersonaFunctor(nombre: Secret[String])

class Secret[A](val value: A) {
  private def hashed: String = {
    val s = value.toString
    val bytes = s.getBytes(StandardCharsets.UTF_8)
    val d = MessageDigest.getInstance("SHA-1")
    val hashBytes = d.digest(bytes)
    new String(hashBytes, StandardCharsets.UTF_8)
  }

  override def toString: String = hashed
}

object Secret {
  implicit val secretFunctor = new Functor[Secret] {
    override def map[A, B](fa: Secret[A])(f: A => B): Secret[B] = new Secret[B](f(fa.value))
  }
}

object TC06Functor extends App {
  println(PersonaFunctor(new Secret[String]("David")))

  val davidSecret: Secret[String] = new Secret[String]("David")
  println(davidSecret.value)

  val upperDavidSecrete: Secret[String] = Functor[Secret].map(davidSecret)(_.toUpperCase)
  println(upperDavidSecrete) // cifrado
  println(upperDavidSecrete.value) //DAVID

  val optFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa match {
      case Some(value) => Some(f(value))
      case None => None
    }
  }
  val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa match {
      case head :: next => f(head) :: map(next)(f)
      case Nil => Nil
    }
  }

  println(optFunctor.map(Some(3))(_ + 1))
  println(listFunctor.map(List(1, 2, 3))(_ + 1))
  println(listFunctor.as(List(1, 2, 3), 10))
}
