package com.kubukoz.stretchthekin

import calico.IOWebApp
import calico.html.io.*
import calico.html.io.given
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import monocle.Focus
import monocle.syntax.all.*

import scala.concurrent.duration.*

extension [A](sigref: SignallingRef[IO, A]) {

  inline def lens[B](inline f: Focus.KeywordContext ?=> A => B): SignallingRef[IO, B] =
    SignallingRef.lens(sigref)(
      _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].get,
      _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].replace,
    )

  inline def sig: Signal[IO, A] = sigref
}

object App extends IOWebApp {

  def render: Resource[IO, HtmlElement[IO]] = SignallingRef[IO].of(0).toResource.flatMap {
    countdownActiveI =>
      val countdowns =
        List.fill(10) {
          CountdownComponent
            .render(
              countdownFrom = 1.seconds,
              refreshRate = 1.second / 60,
              onFinished = IO.println("finished") *> countdownActiveI.update(i => (i + 1) % 10),
            )
        }

      div(
        "Counter ",
        countdownActiveI.map(_.show),
        ":",
        countdowns.liftN(countdownActiveI.sig),
      )

  }

  // enum Step {
  //   case CARS
  //   case PassiveStretch(duration: FiniteDuration)
  //   case PAILS(duration: FiniteDuration)
  //   case RAILS(duration: FiniteDuration)
  //   case Liftoffs(number: Int)
  // }

  // case class Session(steps: List[Step])

  // val sesshn = Session(
  //   List(
  //     Step.CARS,
  //     Step.PassiveStretch(FiniteDuration(30, "seconds")),
  //     Step.PAILS(FiniteDuration(30, "seconds")),
  //     Step.RAILS(FiniteDuration(30, "seconds")),
  //     Step.Liftoffs(10),
  //   )
  // )

}
