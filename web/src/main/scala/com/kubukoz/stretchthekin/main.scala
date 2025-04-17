package com.kubukoz.stretchthekin

import calico.IOWebApp
import calico.html.io.*
import calico.html.io.given
import cats.derived.*
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Queue
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlDivElement
import fs2.dom.HtmlElement
import io.circe.*
import io.circe.syntax.*
import monocle.Focus
import monocle.syntax.all.*
import util.chaining.*

import scala.concurrent.duration.{span as _, *}
import scala.scalajs.js.JSConverters.*

extension [A](sigref: SignallingRef[IO, A]) {

  inline def lens[B](inline f: Focus.KeywordContext ?=> A => B): SignallingRef[IO, B] =
    SignallingRef.lens(sigref)(
      _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].get,
      _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].replace,
    )

  inline def sig: Signal[IO, A] = sigref
}

given Encoder[FiniteDuration] = _.toString.asJson

trait Speaker {
  // canceled speeches get unscheduled but not interrupted
  def speak(text: String): IO[Unit]
}

object Speaker {

  def queued: Resource[IO, Speaker] = Queue.unbounded[IO, IO[Unit]].toResource.flatMap { q =>
    def realSpeak(text: String): IO[Unit] = SpeechSynthesisIO
      .getVoices
      .flatMap { voices =>
        SpeechSynthesisIO.speak(new SpeechSynthesisUtterance(text).tap { u =>
          u.voice =
            voices
              .find(v => v.name.startsWith("Eddy") && v.lang == "en-US")
              .headOption
              .orUndefined
          u.pitch = 0.5
          u.rate = 1.1
        })
      }

    val speaker =
      new Speaker {
        def speak(text: String): IO[Unit] = (IO.ref(false), IO.deferred[Unit]).flatMapN {
          (canceled, completed) =>
            q.offer(
              canceled
                .get
                .ifM(
                  ifTrue = IO.println(s"skipping canceled speech: $text") *> IO.unit,
                  ifFalse = realSpeak(text),
                ) *> completed.complete(()).void
            ) *>
              completed.get.onCancel(canceled.set(true))
        }
      }

    fs2
      .Stream
      .fromQueueUnterminated(q)
      .foreach(identity)
      .compile
      .drain
      .background
      .as(speaker)
  }

  def speak(
    text: String
  )(
    using speaker: Speaker
  ): IO[Unit] = speaker.speak(text)

}

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

        val allScreens = StepV2.kneeIrEr45minute

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
