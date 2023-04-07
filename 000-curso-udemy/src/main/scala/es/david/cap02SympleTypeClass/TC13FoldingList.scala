package es.david.cap02SympleTypeClass

import scala.annotation.tailrec

trait MList[+A]

case class MCons[+A](hd: A, tl: MList[A]) extends MList[A]

case object MNil extends MList[Nothing]

object TC13FoldingList extends App {
  def suma(inst: MList[Int]): Int = {
    @tailrec
    def sumAux(inst: MList[Int], v: Int): Int = {
      inst match {
        case MCons(hd, tl) => sumAux(tl, v + hd)
        case MNil => v
      }
    }

    sumAux(inst, 0)
  }

  def lengthList[A](list: MList[A]): Int = list match {
    case MCons(_, tl) => 1 + lengthList(tl)
    case MNil => 0
  }

  def filterPositivos(inst: MList[Int]): MList[Int] = inst match {
    case MCons(x, xs) =>
      if (x > 0) MCons(x, filterPositivos(xs))
      else filterPositivos(xs)
    case MNil => MNil
  }

  val cola = MCons(3, MCons(2, MCons(-1, MNil)))

  println(suma(cola))
  println(lengthList(cola))
  println(filterPositivos(cola))

  def foldRight[A, B](list: MList[A])(z: B)(f: (A, B) => B): B = {
    list match {
      case MCons(hd, tl) => f(hd, foldRight(tl)(z)(f))
      case MNil => z
    }
  }

  def sumaFold(inst: MList[Int]): Int = {
    foldRight(inst)(0)(_ + _)
  }

  println(sumaFold(cola))

  def lengthListFold[A](list: MList[A]): Int = {
    foldRight(list)(0)((a, acc) => acc + 1)
  }

  println(lengthListFold(cola))


  def filterPositivosFold(inst: MList[Int]): MList[Int] = {
    foldRight(inst)(MNil: MList[Int]) {
      (x, y) => if (x > 0) MCons(x, y) else y
    }
  }
  println(filterPositivosFold(cola))

}
