package zio.web.http

import java.net.URI

sealed trait HttpRequest[+A] { self =>
  def <>[A1 >: A](that: HttpRequest[A1]): HttpRequest[A1] = self.orElse(that)

  def map[B](f: A => B): HttpRequest[B] = HttpRequest.Map(self, f)

  def orElse[A1 >: A](that: HttpRequest[A1]): HttpRequest[A1] =
    self.orElseEither(that).map(_.merge)

  def orElseEither[B](that: HttpRequest[B]): HttpRequest[Either[A, B]] = HttpRequest.OrElseEither(self, that)

  def run(method: String, uri: java.net.URI, version: String, headers: HttpHeaders): Option[A]

  def fold[Z](z: Z)(pf: PartialFunction[(Z, HttpRequest[_]), Z]): Z =
    self match {
      case HttpRequest.Map(request, _)       => request.fold(z)(pf)
      case r: HttpRequest.OrElseEither[_, _] => r.right.fold(r.left.fold(z)(pf))(pf)
      case r: HttpRequest.Zip[_, _]          => r.right.fold(r.left.fold(z)(pf))(pf)
      case request                           => pf.applyOrElse[(Z, HttpRequest[_]), Z]((z, request), _ => z)
    }

  def zip[B](that: HttpRequest[B]): HttpRequest[(A, B)] = HttpRequest.Zip(self, that)

  def zipWith[B, C](that: HttpRequest[B])(f: (A, B) => C): HttpRequest[C] = self.zip(that).map(f.tupled)

  def headers: List[String] = self.fold(List.empty[String]) {
    case (headers, HttpRequest.Header(name)) => headers ++ List(name)
  }
}

object HttpRequest {
  case object Succeed extends HttpRequest[Unit] {
    def run(method: String, uri: java.net.URI, version: String, headers: HttpHeaders): Option[Unit] = Some(())
  }

  case object Fail extends HttpRequest[Nothing] {
    def run(method: String, uri: java.net.URI, version: String, headers: HttpHeaders): Option[Nothing] = None
  }

  case object Method extends HttpRequest[String] {
    def run(method: String, uri: java.net.URI, version: String, headers: HttpHeaders): Option[String] = Some(method)
  }

  final case class Header(name: String) extends HttpRequest[String] {

    def run(method: String, uri: java.net.URI, version: String, headers: HttpHeaders): Option[String] =
      headers.value.get(name)
  }

  final case object URI extends HttpRequest[java.net.URI] {
    def run(method: String, uri: java.net.URI, version: String, headers: HttpHeaders): Option[URI] = Some(uri)
  }

  final case object Version extends HttpRequest[String] {

    def run(method: String, uri: java.net.URI, version: String, headers: HttpHeaders): Option[String] =
      Some(version)
  }

  final case object IpAddress extends HttpRequest[Option[java.net.InetAddress]] {

    def run(
      method: String,
      uri: java.net.URI,
      version: String,
      headers: HttpHeaders
    ): Option[Option[java.net.InetAddress]] =
      Some(
        headers.value
          .get("True-Client-IP")
          .orElse(
            headers.value.get("X-Forwarded-For").flatMap { cs =>
              val c = cs.takeWhile(_ != ',')
              if (c.isEmpty) None else Some(c)
            }
          )
          .map(java.net.InetAddress.getByName)
      )
  }

  final case class Map[A, B](request: HttpRequest[A], f: A => B) extends HttpRequest[B] {

    def run(method: String, uri: java.net.URI, version: String, headers: HttpHeaders): Option[B] =
      request.run(method, uri, version, headers).map(f)
  }

  final case class OrElseEither[A, B](left: HttpRequest[A], right: HttpRequest[B]) extends HttpRequest[Either[A, B]] {

    def run(method: String, uri: java.net.URI, version: String, headers: HttpHeaders): Option[Either[A, B]] = {
      val l = left.run(method, uri, version, headers)
      val r = right.run(method, uri, version, headers)
      l.map(Left(_)).orElse(r.map(Right(_)))
    }
  }

  final case class Zip[A, B](left: HttpRequest[A], right: HttpRequest[B]) extends HttpRequest[(A, B)] {

    def run(method: String, uri: java.net.URI, version: String, headers: HttpHeaders): Option[(A, B)] =
      for {
        l <- left.run(method, uri, version, headers)
        r <- right.run(method, uri, version, headers)
      } yield (l, r)
  }
}
