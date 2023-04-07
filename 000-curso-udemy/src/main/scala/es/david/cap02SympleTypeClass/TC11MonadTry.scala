package es.david.cap02SympleTypeClass

import cats._
import cats.implicits._
import scala.util._

object TC11MonadTry extends App {

  implicit val tryMonad: Monad[Try] = new Monad[Try] {
    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] =
      fa match {
        case Failure(e) => Failure(e)
        case Success(value) => f(value)
      }

    override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???

    override def pure[A](x: A): Try[A] = Success(x)
  }

  println(tryMonad.pure(5))
  println(tryMonad.pure(5).flatMap(i => tryMonad.pure(i + 1)).flatMap(i => tryMonad.pure(i + 1)))
  println(tryMonad.pure(5).flatMap(i => Failure(new Exception("Booom!"))))
}
