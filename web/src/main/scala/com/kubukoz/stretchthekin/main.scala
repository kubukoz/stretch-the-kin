package com.kubukoz.stretchthekin

import calico.IOWebApp
import calico.html.io.*
import calico.html.io.given
import cats.derived.*
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import io.circe.*
import io.circe.syntax.*
import monocle.Focus
import monocle.syntax.all.*

import scala.concurrent.duration.{span as _, *}

extension [A](sigref: SignallingRef[IO, A]) {

  inline def lens[B](inline f: Focus.KeywordContext ?=> A => B): SignallingRef[IO, B] =
    SignallingRef.lens(sigref)(
      _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].get,
      _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].replace,
    )

  inline def sig: Signal[IO, A] = sigref
}

given Encoder[FiniteDuration] = _.toString.asJson

object App extends IOWebApp {

  enum AppState derives Eq {
    case Unstarted
    case Active(i: Int)
    case Finished
  }

  def render: Resource[IO, HtmlElement[IO]] =
    (
      SignallingRef[IO]
        .of(AppState.Unstarted)
        .toResource,
      Speaker.queued,
      Sounds.make,
    )
      .flatMapN { (appState, speaker, sounds) =>
        given Speaker = speaker
        given Sounds = sounds

        val allScreens = Step.kneeIrEr45minute

        div(
          s"screen count: ${allScreens.size}, total time: ${allScreens.totalTime.toMinutes}m",
          appState.map {
            case AppState.Unstarted => div("Check voice to start")
            case AppState.Active(i) =>
              val remainingStepsIncludingThis = allScreens.drop(i)

              div(
                s"screen ${i + 1}/${allScreens.size}, remaining time: ${remainingStepsIncludingThis.totalTime.toMinutes}m",
                ScreenComponentV2.render(
                  remainingStepsIncludingThis.head,
                  onFinished = appState.update {
                    case AppState.Active(i) if i < allScreens.size - 1 => AppState.Active(i + 1)
                    case _                                             => AppState.Finished
                  },
                ),
              )
            case AppState.Finished => Sounds.playFinished.background *> div("Finished!")
          },
          SignallingRef[IO].of(false).toResource.flatMap { clicked =>
            button(
              disabled <-- (clicked, appState.map(_ =!= AppState.Unstarted)).mapN(_ || _),
              "Check voice",
              onClick {
                clicked.set(true) *>
                  Sounds.playEnding.surround {
                    Speaker.speak("Starting the session")
                  } *>
                  appState.set(AppState.Active(0))
              },
            )
          },
          ul(
            allScreens.zipWithIndex.map { (screen, i) =>
              li(
                a(
                  href := "#",
                  s"$i: ${screen.title} (${screen.content.totalTime.toSeconds}s)",
                  onClick(appState.set(AppState.Active(i))),
                ),
                detailsTag(
                  summaryTag("Details"),
                  pre(code(screen.content.asJson.spaces2)),
                ),
              )
            }
          ),
        )
      }

}
