package es.david.cap02SympleTypeClass

import cats._
import cats.implicits._

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
}
