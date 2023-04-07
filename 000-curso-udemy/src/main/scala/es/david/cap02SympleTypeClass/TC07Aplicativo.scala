package es.david.cap02SympleTypeClass

import cats.Applicative
import cats.implicits.{catsSyntaxTuple3Semigroupal, catsSyntaxTuple4Semigroupal}

import cats._
import cats.implicits._

object EjemploTeoria extends App {
  sealed trait MyValidated[+A]

  case class MyValid[+A](a: A) extends MyValidated[A]

  case class MyInvalid(error: List[String]) extends MyValidated[Nothing]

  def validadteName(name: String): MyValidated[String] =
    if (name.forall(_.isLetter)) MyValid(name)
    else MyInvalid(List("El nombre solo puede contener letras"))

  println(validadteName("David"))
  println(validadteName("D4v1d"))

  def validarEdad(edad: Int): MyValidated[Int] =
    if (edad >= 18) MyValid(edad)
    else MyInvalid(List("Tiene que ser mayor que 18"))

  println(validarEdad(-1))
  println(validarEdad(11))

  case class PersonaValid(nombre: String, edad: Int)

  def validarPersonaSol1(persona: PersonaValid): MyValidated[PersonaValid] =
    (validadteName(persona.nombre), validarEdad(persona.edad)) match {
      case (MyValid(a), MyValid(b)) => MyValid(PersonaValid(a, b))
      case (MyInvalid(e1), MyValid(_)) => MyInvalid(e1)
      case (MyValid(_), MyInvalid(e2)) => MyInvalid(e2)
      case (MyInvalid(e1), MyInvalid(e2)) => MyInvalid(e1 ++ e2)
    }

  def map2[A, B, C](va: MyValidated[A], vb: MyValidated[B])(f: (A, B) => C): MyValidated[C] = {
    (va, vb) match {
      case (MyValid(a), MyValid(b)) => MyValid(f(a, b))
      case (MyInvalid(e1), MyValid(_)) => MyInvalid(e1)
      case (MyValid(_), MyInvalid(e2)) => MyInvalid(e2)
      case (MyInvalid(e1), MyInvalid(e2)) => MyInvalid(e1 ++ e2)
    }
  }
  /*
  A -----> F[A]
  |        |
 f|        | ap(pure(f))
  |        |
 \./      \./
  B -----> F[B]
     pure
  */

  implicit val aplicativo: Applicative[MyValidated] = new Applicative[MyValidated] {
    override def pure[A](x: A): MyValidated[A] = MyValid(x)

    override def ap[A, B](ff: MyValidated[A => B])(fa: MyValidated[A]): MyValidated[B] =
      (ff, fa) match {
        case (MyValid(f), MyValid(a)) => MyValid(f(a))
        case (MyInvalid(e1), MyValid(_)) => MyInvalid(e1)
        case (MyValid(_), MyInvalid(e2)) => MyInvalid(e2)
        case (MyInvalid(e1), MyInvalid(e2)) => MyInvalid(e1 ++ e2)
      }


    override def map2[A0, A1, Z](fa: MyValidated[A0], fb: MyValidated[A1])(f: (A0, A1) => Z): MyValidated[Z] =
      (fa, fb) match {
        case (MyValid(a), MyValid(b)) => MyValid(f(a, b))
        case (MyInvalid(e1), MyValid(_)) => MyInvalid(e1)
        case (MyValid(_), MyInvalid(e2)) => MyInvalid(e2)
        case (MyInvalid(e1), MyInvalid(e2)) => MyInvalid(e1 ++ e2)
      }

    def apAlternativoUsnadoMap2[A, B](ff: MyValidated[A => B])(fa: MyValidated[A]): MyValidated[B] =
      map2(ff, fa)((f, a) => f(a))

    def map2ApPure[A0, A1, Z](fa: MyValidated[A0], fb: MyValidated[A1])(f: (A0, A1) => Z): MyValidated[Z] = {
      val g: A0 => A1 => Z = f.curried
      val pureG: MyValidated[A0 => A1 => Z] = pure(g)
      val apPureG: MyValidated[A0] => MyValidated[A1 => Z] = ap(pure(g))

      ap(ap(pure(g))(fa))(fb)
    }

    def tuple[A, B](f1: MyValidated[A], f2: MyValidated[B]): MyValidated[(A, B)] = {
      map2(f1, f2)((a, b) => (a, b))
    }

    // Se puede ver un patrón en los map del aplicativo, ver los A´s

    override def map3[A0, A1, A2, Z](f0: MyValidated[A0], f1: MyValidated[A1], f2: MyValidated[A2])(f: (A0, A1, A2) => Z): MyValidated[Z] = ???

    override def map4[A0, A1, A2, A3, Z](f0: MyValidated[A0], f1: MyValidated[A1], f2: MyValidated[A2], f3: MyValidated[A3])(f: (A0, A1, A2, A3) => Z): MyValidated[Z] = ???
  }


  val v1: MyValidated[Int] = Applicative[MyValidated].pure(1)
  val v2: MyValidated[Int] = Applicative[MyValidated].pure(2)
  val v3: MyValidated[Int] = Applicative[MyValidated].pure(3)
  val v4: MyValidated[Int] = validarEdad(0)
  println(v1)

  // Comprueba si son validos
  println((v1, v2, v3).mapN((a, b, c) => a + b + c))

  // Comprueba si son validos, como el v4 no lo es da error
  println((v1, v2, v3, v4).mapN((a, b, c, d) => a + b + c + d))

}

object TC07Aplicativo extends App {
  val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def pure[A](x: A): Option[A] = Some(x)

    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = {
      (ff, fa) match {
        case (Some(f), Some(a)) => Some(f(a))
        case _ => None
      }
    }
  }

  println(optionApplicative.map2(Some(2), Some(4))(_ + _))
  println(optionApplicative.map2[Int, Int, Int](Some(2), Some(4))(_ + _))
  println(optionApplicative.map2[Int, Int, Int](None, Some(4))(_ + _))

  val listApplicative: Applicative[List] = new Applicative[List] {
    override def pure[A](x: A): List[A] = List(x)

    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] = {
      // producto cartesiano
      (ff, fa) match {
        case (f :: fs, a :: as) => (a :: as).fmap(f) ++ ap(fs)(a :: as)
        case _ => Nil
      }
    }
  }

  println(listApplicative.map2(List(1, 2, 3), List(4, 5))(_ * _))
  println(listApplicative.map2[Int, Int, Int](List(1, 2, 3), Nil)(_ * _))

}
