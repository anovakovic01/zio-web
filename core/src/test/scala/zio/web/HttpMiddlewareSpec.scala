import java.net.URI

import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.test.Assertion._
import zio.test._
import zio.web.http

object HttpMiddlewareSpec extends DefaultRunnableSpec with http.HttpProtocolModule {
  import http.HttpMiddleware

  // TODO: should read logs from file and assert content
  def spec =
    suite("HttpMiddleware")(
      suite("logging")(
        testM("write to file") {
          val dest = HttpMiddleware.fileSink("test.log")

          for {
            l      <- HttpMiddleware.logging(dest).make
            result <- l.request.processor(("GET", new URI("http://zio.dev")))
            _      <- ZIO.effect { println(result) }
          } yield assert(true)(equalTo(true))
        }
      )
    ).provideCustomLayerShared(Blocking.live ++ Clock.live)

}
