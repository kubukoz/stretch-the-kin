import calico.IOWebApp
import calico.html.io.*
import calico.html.io.given
import cats.effect.IO
import cats.effect.kernel.Resource
import fs2.dom.HtmlElement

object App extends IOWebApp {

  def render: Resource[IO, HtmlElement[IO]] = div("hello there")
}
