package es.david.cap02SympleTypeClass

import cats._
import cats.implicits._

import scala.annotation.tailrec

object TC09MonadList extends App {
  val result: List[Int] = for {
    a <- List(1, 2, 3)
    b <- List(4, 5, 6)
  } yield a + b // producto cartesiano
  println(result)

  @tailrec
  val listMonad: Monad[List] = new Monad[List] {
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa match {
      case head :: next => f(head) ::: flatMap(next)(f)
      case Nil => Nil
    }

    override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = ???

    override def pure[A](x: A): List[A] = List(x)
  }

  println(listMonad.flatMap(List(1, 2, 3))(a => List(a + 1, a + 2)))
  // List(2, 3, 3, 4, 4, 5) = List(1+1, 2+1, 1+2, 2+2, 3+1, 3+2)

}
