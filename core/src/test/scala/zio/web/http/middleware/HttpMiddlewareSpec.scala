package zio.web.http.middleware

import zio._
import zio.blocking.Blocking
import zio.duration._
import zio.stream.{ ZSink, ZStream }
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestClock
import zio.web.http.HttpHeaders

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
                l       <- logging(dest).make
                _       <- TestClock.setTime(0.seconds)
                state   <- l.runRequest(method, new URI(uri), version, HttpHeaders(Map(clientHeader -> ipAddr)))
                _       <- l.runResponse(state, status, HttpHeaders(Map(contentLengthHeader -> length.toString)))
                _       <- ZIO.succeed(out.size()).repeatUntil(_ > 0)
                content = new String(out.toByteArray, StandardCharsets.UTF_8)
              } yield assert(content)(
                equalTo(
                  s"$ipAddr - - [01/Jan/1970:00:00:00 +0000] ${"\""}$method $uri $version${"\""} $status $length\n"
                )
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
                l       <- logging(dest).make
                _       <- TestClock.setTime(0.seconds)
                state   <- l.runRequest(method, new URI(uri), version, HttpHeaders(Map(forwardedHeader -> ipAddr)))
                _       <- l.runResponse(state, status, HttpHeaders(Map(contentLengthHeader -> length.toString)))
                _       <- ZIO.succeed(out.size()).repeatUntil(_ > 0)
                content = new String(out.toByteArray, StandardCharsets.UTF_8)
              } yield assert(content)(
                equalTo(
                  s"$ipAddr - - [01/Jan/1970:00:00:00 +0000] ${"\""}$method $uri $version${"\""} $status $length\n"
                )
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
                l       <- logging(dest).make
                _       <- TestClock.setTime(0.seconds)
                state   <- l.runRequest(method, new URI(uri), version, HttpHeaders.empty)
                _       <- l.runResponse(state, status, HttpHeaders(Map(contentLengthHeader -> length.toString)))
                _       <- ZIO.succeed(out.size()).repeatUntil(_ > 0)
                content = new String(out.toByteArray, StandardCharsets.UTF_8)
              } yield assert(content)(
                equalTo(s"- - - [01/Jan/1970:00:00:00 +0000] ${"\""}$method $uri $version${"\""} $status $length\n")
              )
          }
        },
        testM("to the file") {
          ZManaged
            .make(ZIO.succeed(logFile))(path => ZIO.effect(new File(path).delete()).orDie)
            .use {
              path =>
                for {
                  l       <- fileLogging(path).make
                  _       <- TestClock.setTime(0.seconds)
                  state   <- l.runRequest(method, new URI(uri), version, HttpHeaders(Map(clientHeader -> ipAddr)))
                  _       <- l.runResponse(state, status, HttpHeaders(Map(contentLengthHeader -> length.toString)))
                  result  <- ZStream.fromFile(Paths.get(path), 32).runCollect
                  content = new String(result.toArray, StandardCharsets.UTF_8)
                } yield assert(content)(
                  equalTo(
                    s"$ipAddr - - [01/Jan/1970:00:00:00 +0000] ${"\""}$method $uri $version${"\""} $status $length\n"
                  )
                )
            }
        }
      )
    ).provideCustomLayerShared(Blocking.live)

  val clientHeader        = "True-Client-IP"
  val forwardedHeader     = "X-Forwarded-For"
  val contentLengthHeader = "Content-Length"
  val method              = "GET"
  val uri                 = "http://zio.dev"
  val version             = "HTTP/1.1"
  val ipAddr              = "127.0.0.1"
  val status              = 200
  val length              = 1000
  val logFile             = "test.log"
}
