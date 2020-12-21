package zio.web.http

import zio._
import zio.blocking.Blocking
import zio.clock._
import zio.stream.{ Sink, ZSink, ZStream }
import zio.web.http.HttpMiddleware._

import java.io.{ FileOutputStream, IOException, OutputStream }
import java.time.format.DateTimeFormatter

package object middleware {

  def logging[R, E](sink: ZSink[R, E, String, Byte, Long]): HttpMiddleware[Clock with R, Nothing] = {
    val formatter = DateTimeFormatter.ofPattern("dd/MMM/yyyy:HH:mm:ss Z")

    HttpMiddleware(
      for {
        queue  <- Queue.bounded[String](128)
        stream = ZStream.fromQueue(queue)
        _      <- stream.run(sink).fork
      } yield Middleware(
        request(HttpRequest.Method.zip(HttpRequest.URI).zip(HttpRequest.Version).zip(HttpRequest.IpAddress)) {
          case (((method, uri), version), ipAddr) =>
            val ipStr = ipAddr.fold("-")(_.getHostAddress)
            currentDateTime
              .fold(
                _ => s"$ipStr - - - ${"\""}$method $uri $version${"\""}",
                now => {
                  val time = now.format(formatter)
                  s"$ipStr - - [$time] ${"\""}$method $uri $version${"\""}"
                }
              )
        },
        Response(
          HttpResponse.StatusCode.zip(HttpResponse.Header("Content-Length")),
          (state: String, resp: (Int, String)) => queue.offer(s"$state ${resp._1} ${resp._2}\n").as(Patch.empty)
        )
      )
    )
  }

  def fileLogging[R](path: String): HttpMiddleware[Clock with Blocking, Nothing] =
    logging(fileSink(path))

  private[middleware] def fileSink(path: String): ZSink[Blocking, Throwable, String, Byte, Long] = {
    val stream: ZManaged[Blocking, IOException, OutputStream] =
      ZManaged
        .fromAutoCloseable(
          ZIO.effect(new FileOutputStream(path, true))
        )
        .refineToOrDie[IOException]

    Sink
      .fromOutputStreamManaged(stream)
      .contramapChunks[String](_.flatMap(str => Chunk.fromIterable(str.getBytes)))
  }
}
