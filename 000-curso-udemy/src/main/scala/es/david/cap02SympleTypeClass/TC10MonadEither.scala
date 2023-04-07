package es.david.cap02SympleTypeClass

import cats._
import cats.implicits._

object TC10MonadEither extends App {
  // No podemos hacer eso * -> * -> *
  // implicit val eitherMonad:Monad[Either] = ???

  //  implicit val eitherMonad: Monad[Either[String, *]] = new Monad[Either[String, *]] {
  //    override def flatMap[A, B](fa: Either[String, A])(f: A => Either[String, B]): Either[String, B] = ???
  //
  //    override def tailRecM[A, B](a: A)(f: A => Either[String, Either[A, B]]): Either[String, B] = ???
  //
  //    override def pure[A](x: A): Either[String, A] = Right(x)
  //  }

  implicit def eitherMonad[E]: Monad[Either[E, *]] = new Monad[Either[E, *]] {
    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      fa match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???

    override def pure[A](x: A): Either[E, A] = Right(x)
  }

  val cincoLeftString: Either[String, Int] = 5.asRight[String]
  val cincoLeftStringMas1: Either[String, Int] = 5.asRight[String].flatMap(x => (x + 1).asRight[String])
  val cincoLeftStringMas1For: Either[String, Int] = for {
    cinco <- 5.asRight[String]
    res <- (cinco + 1).asRight[String]
  } yield res
  println(cincoLeftString)
  println(cincoLeftStringMas1)
  println(cincoLeftStringMas1For)

  val boomLeft: Either[String, Int] = "boom".asLeft[Int]
  println(cincoLeftString.flatMap(x => boomLeft))
  println(cincoLeftString.flatMap(x => boomLeft).flatMap(x => "este boom no se ve".asLeft[Int]))
}
