package zio.web.middleware

import zio._
import zio.blocking.Blocking
import zio.duration._
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestClock
import zio.web.http.{ HttpHeaders, HttpMiddleware }

import java.io.File
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.Paths

object HttpMiddlewareSpec extends DefaultRunnableSpec {

  def spec =
    suite("HttpMiddleware")(
      suite("logging")(
        testM("write to file") {
          val method    = "GET"
          val uri       = "http://zio.dev"
          val ipAddress = "127.0.0.1"
          val status    = 200
          val length    = 1000
          val dest      = HttpMiddleware.fileSink(logFile)

          for {
            l       <- HttpMiddleware.logging(dest).make
            _       <- TestClock.setTime(0.seconds)
            state   <- l.runRequest("GET", new URI(uri), HttpHeaders(Map("True-Client-IP" -> ipAddress)))
            _       <- l.runResponse(state, status, HttpHeaders(Map("Content-Length" -> length.toString)))
            result  <- ZStream.fromFile(Paths.get(logFile), 32).runCollect
            content = new String(result.toArray, StandardCharsets.UTF_8)
            _       <- ZIO.effect(new File(logFile).delete()).orDie
          } yield assert(content)(
            equalTo(s"$ipAddress - - [01/Jan/1970:00:00:00 +0000] \'$method $uri\' $status $length\n")
          )
        }
      )
    ).provideCustomLayerShared(Blocking.live)

  val logFile = "test.log"
}
