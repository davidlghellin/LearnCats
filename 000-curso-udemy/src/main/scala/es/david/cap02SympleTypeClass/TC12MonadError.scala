package es.david.cap02SympleTypeClass

import cats._
import cats.implicits._
import es.david.cap02SympleTypeClass.HttpMethod.doRequest

import java.io.IOException
import scala.util.{Failure, Success, Try}

trait HttpMethod

case object GET extends HttpMethod

case class HtttpRequest(method: HttpMethod, url: String)

case class HttpResponse(status: Int)

object HttpMethod {
  def doRequest(req: HtttpRequest): HttpResponse =
    if (math.random() < 0.5) throw new IOException("Boom!")
    else HttpResponse(200)

  def executeRequest(req: HtttpRequest): Option[HttpResponse] =
    try {
      Some(doRequest(req))
    } catch {
      case _: Exception => None
    }

  def executeRequest2(req: HtttpRequest): Either[String, HttpResponse] =
    try {
      Right(doRequest(req))
    } catch {
      case _: Exception => Left("Sorry :(")
    }

  def executeRequest3(req: HtttpRequest): Try[HttpResponse] =
    try {
      Success(doRequest(req))
    } catch {
      case e: Exception => Failure(e)
    }

  // tenemos 3 tipos para devolver estas funciones Option, Either, Try
}

object TC12MonadError extends App {
  println(HttpMethod.executeRequest(HtttpRequest(GET, "una.web")))
  println(HttpMethod.executeRequest3(HtttpRequest(GET, "una.web")))


  val optionME: MonadError[Option, Unit] = new MonadError[Option, Unit] {
    override def pure[A](x: A): Option[A] = Some(x)

    override def raiseError[A](e: Unit): Option[A] = None

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)

    override def handleErrorWith[A](fa: Option[A])(f: Unit => Option[A]): Option[A] = fa.orElse(f(()))

    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = ???
  }

  def eitherME[E]: MonadError[Either[E, *], E] = new MonadError[Either[E, *], E] {
    override def pure[A](x: A): Either[E, A] = Right(x)

    override def raiseError[A](e: E): Either[E, A] = Left(e)

    override def handleErrorWith[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] =
      fa match {
        case Left(e) => f(e)
        case Right(value) => Right(value)
      }

    override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ???

    override def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = ???
  }

  val tryME: MonadError[Try, Throwable] = new MonadError[Try, Throwable] {
    override def pure[A](x: A): Try[A] = Success(x)

    override def raiseError[A](e: Throwable): Try[A] = Failure(e)

    override def handleErrorWith[A](fa: Try[A])(f: Throwable => Try[A]): Try[A] = fa match {
      case Failure(exception) => f(exception)
      case Success(value) => Success(value)
    }

    override def flatMap[A, B](fa: Try[A])(f: A => Try[B]): Try[B] = ???

    override def tailRecM[A, B](a: A)(f: A => Try[Either[A, B]]): Try[B] = ???
  }

  def executeRequesME[F[_]](request: HtttpRequest)(implicit ME: MonadError[F, Throwable]): F[HttpResponse] = {
    try {
      ME.pure(doRequest(request))
    } catch {
      case e: Exception => ME.raiseError(e)
    }
  }

  println(executeRequesME[Try](HtttpRequest(GET, "una.web")))
  type ErrorOr[A] = Either[Throwable, A]
  println(executeRequesME[ErrorOr](HtttpRequest(GET, "una.web")))

  // no podemos con error
  //  println(executeRequesME[Option](HtttpRequest(GET, "una.web")))
  def executeRequesME2[F[_], E](request: HtttpRequest)(f: Exception => E)(implicit ME: MonadError[F, E]): F[HttpResponse] = {
    try {
      ME.pure(doRequest(request))
    } catch {
      case e: Exception => ME.raiseError(f(e))
    }
  }

  println(executeRequesME2[Option, Unit](HtttpRequest(GET, "una.web"))((e: Exception) => ()))
  type ErrorOrString[A] = Either[String, A]
  println(executeRequesME2[ErrorOrString, String](HtttpRequest(GET, "una.web"))((e: Exception) => e.getMessage))

}
