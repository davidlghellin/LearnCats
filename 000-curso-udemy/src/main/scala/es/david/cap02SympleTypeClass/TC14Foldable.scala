package es.david.cap02SympleTypeClass

import cats._
import cats.implicits._

trait MListF[+A]

object MListF {
  def apply[A](elems: A*): MListF[A] = {
    elems.foldRight(mnil[A])((a, b) => mcons(a, b))
  }

  case class MCons[+A](hd: A, tl: MListF[A]) extends MListF[A]

  case object MNil extends MListF[Nothing]

  def mnil[A]: MListF[A] = MNil

  def mcons[A](hd: A, tl: MListF[A]): MListF[A] = MCons(hd, tl)

  implicit val listFoldable: Foldable[MListF] = new Foldable[MListF] {
    override def foldLeft[A, B](fa: MListF[A], b: B)(f: (B, A) => B): B = fa match {
      case MNil => b
      case MCons(h, t) => foldLeft(t, f(b, h))(f)
    }

    override def foldRight[A, B](fa: MListF[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
      def loop(as: MListF[A]): Eval[B] =
        as match {
          case MNil => lb
          case MCons(h, t) => f(h, Eval.defer(loop(t)))
        }

      Eval.defer(loop(fa))
    }
  }
}

object TC14Foldable extends App {

  println(Foldable[List].foldMap(List(1, 2, 3))(_.show))
  println(Foldable[List].foldMap(List("a", "b", "c"))(_.length))


  import MListF._

  MListF(1, 2, 3)

  def sum(ints: MListF[Int]): Int =
  //    ints.foldLeft(0)((b, a) => b + a)
    Foldable[MListF].foldLeft(ints, 0)((b, a) => b + a)

  def length[A](list: MListF[A]): Int =
  //    Foldable[MListF].foldLeft(list,0)((b, a) => 1 + b)
    list.foldLeft(0)((b, a) => 1 + b)

  def filterPositive(ints: MListF[Int]): MListF[Int] =
    ints.foldRight(Eval.now(mnil[Int]))((i, eis) => if (i > 0) Eval.now(mcons(i, eis.value)) else eis).value

  println(sum(MListF(1, 2, 3)))
  println(length(MListF(1, 2, 3)))
  println(filterPositive(MListF(1, -2, 3)))

  println(MListF(1, 2, 3).foldMap(_.show))

  def find[F[_] : Foldable, A](fa: F[A])(p: A => Boolean): Option[A] = {
    //    fa match {
    //      case MNil => None
    //      case MCons(h, t) => if (p(h)) Some(h) else find(t)
    //    }
    fa.foldLeft[Option[A]](None)((b, a) => if (p(a)) Some(a) else b)
  }

  // Encuentra el último
  println(find[MListF, Int](MListF(1, 2, 3, 4))(i => i % 2 == 0))
  println(find[MListF, Int](MListF(1, 3))(i => i % 2 == 0))


  def exits[F[_] : Foldable, A](fa: F[A])(p: A => Boolean): Boolean = {
    //        fa match {
    //          case MNil => false
    //          case MCons(h, t) => if (p(h)) true else exits(t)
    //        }
    // find(fa)(p).nonEmpty
    fa.foldLeft[Boolean](false)((b, actual) => if (p(actual)) true else false)
  }

  println(exits[MListF, Int](MListF(1, 2, 3, 4))(i => i % 2 == 0))
  println(exits[MListF, Int](MListF(1, 3))(i => i % 2 == 0))


  //
  def toList[F[_] : Foldable, A](fa: F[A]): MListF[A] =
  //    fa match {
  //      case MNil => MNil
  //      case MCons(h, t) => MCons(h, toList(t))
  //    }
  //    fa.foldLeft[MListF[A]](MNil)((b, a) => MCons(a, b))
    fa.foldRight[MListF[A]](Eval.now(MNil))((a, eb) => Eval.now(MCons(a, eb.value))).value

  println(toList[MListF, Int](MListF(1, 2, 3, 4))) // MCons(4,MCons(3,MCons(2,MCons(1,MNil))))

  def forAll[F[_] : Foldable, A](fa: F[A])(p: A => Boolean): Boolean = {
    //    fa match {
    //      case MNil => true
    //      case MCons(h, t) => if (p(p)) forAll(t)(p) else false
    //    }
    fa.foldLeft[Boolean](true)((b, actual) => if (p(actual)) b else false)
  }

  println(forAll[MListF, Int](MListF(1, 2, 3, 4))(i => i % 2 == 0))
  println(forAll[MListF, Int](MListF(2, 4))(i => i % 2 == 0))

}
