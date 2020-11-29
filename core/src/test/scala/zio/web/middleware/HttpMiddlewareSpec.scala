import java.io.File
import java.net.{ InetAddress, URI }
import java.nio.charset.StandardCharsets

import zio.blocking.Blocking
import zio.clock.Clock
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._
import zio.web.http.HttpMiddleware
import zio._

object HttpMiddlewareSpec extends DefaultRunnableSpec {

  // TODO: should read logs from file and assert content
  def spec =
    suite("HttpMiddleware")(
      suite("logging")(
        testM("write to file") {
          val dest = HttpMiddleware.fileSink(logFile)

          for {
            l       <- HttpMiddleware.logging(dest).make
            ipAddr  = InetAddress.getLocalHost
            state   <- l.request.run((("GET", new URI("http://zio.dev")), ipAddr))
            _       <- l.response.run(state, (200, 1000))
            result  <- ZStream.fromFile(dest).runCollect
            content = new String(result.toArray, StandardCharsets.UTF_8)
          } yield assert(content)(equalTo(""))
        }
      ).provideManagedShared(Managed.make(ZIO.effect(new File(logFile)))(file => ZIO.effect(file.delete()).orDie))
    ).provideCustomLayerShared(Blocking.live ++ Clock.live)

  val logFile = "test.log"
}
