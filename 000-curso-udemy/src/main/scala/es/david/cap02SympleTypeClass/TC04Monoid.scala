package es.david.cap02SympleTypeClass

import cats._
import cats.implicits._

case class VelocidadMonoid(metrosPorSegundo: Double) {
  def kilometrosPorSegundos: Double = metrosPorSegundo / 1000.0

  def millasPorSegundos: Double = metrosPorSegundo / 1609.34
}

object VelocidadMonoid {
  def sumaVelocidades(s1: VelocidadMonoid, s2: VelocidadMonoid): VelocidadMonoid = {
    VelocidadMonoid(s1.metrosPorSegundo + s2.metrosPorSegundo)
  }

  // Asociativo pero no tiene que ser conmutativo, puede serlo
  // Si es conmutativo es un monoide conmutativo o abeliano

  implicit val monoidVelocidadCorto2: Monoid[VelocidadMonoid] = Monoid.instance(VelocidadMonoid(0), sumaVelocidades)

  object Instances {

    implicit val monoidVelocidad: Monoid[VelocidadMonoid] = new Monoid[VelocidadMonoid] {
      override def empty: VelocidadMonoid = VelocidadMonoid(0)

      override def combine(x: VelocidadMonoid, y: VelocidadMonoid): VelocidadMonoid = sumaVelocidades(x, y)
    }

    implicit val monoidVelocidadCorto: Monoid[VelocidadMonoid] = Monoid.instance(VelocidadMonoid(0), (x, y) => sumaVelocidades(x, y))
  }

  implicit val eqSpeed: Eq[VelocidadMonoid] = Eq.fromUniversalEquals
}

object TC04Monoid extends App {
  println(Monoid[VelocidadMonoid].combine(VelocidadMonoid(10000), VelocidadMonoid(10000)))

  println(Monoid[VelocidadMonoid].empty)

  println(VelocidadMonoid(10000) |+| VelocidadMonoid(10000))

  println(Monoid[VelocidadMonoid].combineAll(List(VelocidadMonoid(10000), VelocidadMonoid(10000), VelocidadMonoid(10000))))
  println(List(VelocidadMonoid(10000), VelocidadMonoid(10000), VelocidadMonoid(10000)).combineAll)

  // Para saber si es el monoid el valor neutro
  // necesitamos añadir la de Eq[VelocidadMonoid]
  println(Monoid[VelocidadMonoid].isEmpty(VelocidadMonoid(2)))

  // ejercicios
  val sumMonoid: Monoid[Int] = Monoid.instance(0, _ + _)
  val minMonoid: Monoid[Int] = Monoid.instance(Int.MaxValue, _ min _)

  def listMonoid[A]: Monoid[List[A]] = Monoid.instance(Nil, _ ++ _)

  val strMonoid: Monoid[String] = Monoid.instance("", _ + _)

  println(sumMonoid.combine(4, 5))
  println(minMonoid.combine(4, 5))
  println(strMonoid.combine("4", "5"))
}
