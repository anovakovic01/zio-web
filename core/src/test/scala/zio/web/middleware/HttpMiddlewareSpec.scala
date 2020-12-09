package zio.web.middleware

import zio._
import zio.blocking.Blocking
import zio.duration._
import zio.stream.{ ZSink, ZStream }
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestClock
import zio.web.http.{ HttpHeaders, HttpMiddleware }

import java.io.{ ByteArrayOutputStream, File }
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Paths

object HttpMiddlewareSpec extends DefaultRunnableSpec {

  def spec =
    suite("HttpMiddleware")(
      suite("logging")(
        testM("with the True-Client-IP header") {
          ZManaged.fromAutoCloseable(ZIO.succeed(new ByteArrayOutputStream())).use {
            out =>
              val dest = ZSink
                .fromOutputStream(out)
                .contramapChunks[String](_.flatMap(str => Chunk.fromIterable(str.getBytes)))

              for {
                l       <- HttpMiddleware.logging(dest).make
                _       <- TestClock.setTime(0.seconds)
                state   <- l.runRequest(method, new URI(uri), HttpHeaders(Map(clientHeader -> ipAddr)))
                _       <- l.runResponse(state, status, HttpHeaders(Map(contentLengthHeader -> length.toString)))
                _       <- ZIO.succeed(out.size()).repeatUntil(_ > 0)
                content = new String(out.toByteArray, StandardCharsets.UTF_8)
              } yield assert(content)(
                equalTo(s"$ipAddr - - [01/Jan/1970:00:00:00 +0000] ${"\""}$method $uri${"\""} $status $length\n")
              )
          }
        },
        testM("with the X-Forwarded-For header") {
          ZManaged.fromAutoCloseable(ZIO.succeed(new ByteArrayOutputStream())).use {
            out =>
              val dest = ZSink
                .fromOutputStream(out)
                .contramapChunks[String](_.flatMap(str => Chunk.fromIterable(str.getBytes)))

              for {
                l       <- HttpMiddleware.logging(dest).make
                _       <- TestClock.setTime(0.seconds)
                state   <- l.runRequest(method, new URI(uri), HttpHeaders(Map(forwardedHeader -> ipAddr)))
                _       <- l.runResponse(state, status, HttpHeaders(Map(contentLengthHeader -> length.toString)))
                _       <- ZIO.succeed(out.size()).repeatUntil(_ > 0)
                content = new String(out.toByteArray, StandardCharsets.UTF_8)
              } yield assert(content)(
                equalTo(s"$ipAddr - - [01/Jan/1970:00:00:00 +0000] ${"\""}$method $uri${"\""} $status $length\n")
              )
          }
        },
        testM("without IP address") {
          ZManaged.fromAutoCloseable(ZIO.succeed(new ByteArrayOutputStream())).use {
            out =>
              val dest = ZSink
                .fromOutputStream(out)
                .contramapChunks[String](_.flatMap(str => Chunk.fromIterable(str.getBytes)))

              for {
                l       <- HttpMiddleware.logging(dest).make
                _       <- TestClock.setTime(0.seconds)
                state   <- l.runRequest(method, new URI(uri), HttpHeaders.empty)
                _       <- l.runResponse(state, status, HttpHeaders(Map(contentLengthHeader -> length.toString)))
                _       <- ZIO.succeed(out.size()).repeatUntil(_ > 0)
                content = new String(out.toByteArray, StandardCharsets.UTF_8)
              } yield assert(content)(
                equalTo(s"- - - [01/Jan/1970:00:00:00 +0000] ${"\""}$method $uri${"\""} $status $length\n")
              )
          }
        },
        testM("to the file") {
          val dest = HttpMiddleware.fileSink(logFile)

          for {
            l       <- HttpMiddleware.logging(dest).make
            _       <- TestClock.setTime(0.seconds)
            state   <- l.runRequest(method, new URI(uri), HttpHeaders(Map(clientHeader -> ipAddr)))
            _       <- l.runResponse(state, status, HttpHeaders(Map(contentLengthHeader -> length.toString)))
            result  <- ZStream.fromFile(Paths.get(logFile), 32).runCollect
            content = new String(result.toArray, StandardCharsets.UTF_8)
            _       <- ZIO.effect(new File(logFile).delete()).orDie
          } yield assert(content)(
            equalTo(s"$ipAddr - - [01/Jan/1970:00:00:00 +0000] ${"\""}$method $uri${"\""} $status $length\n")
          )
        }
      )
    ).provideCustomLayerShared(Blocking.live)

  val clientHeader        = "True-Client-IP"
  val forwardedHeader     = "X-Forwarded-For"
  val contentLengthHeader = "Content-Length"
  val method              = "GET"
  val uri                 = "http://zio.dev"
  val ipAddr              = "127.0.0.1"
  val status              = 200
  val length              = 1000
  val logFile             = "test.log"
}
