package com.kubukoz.stretchthekin

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.Queue
import cats.syntax.all.*
import util.chaining.*

import scala.concurrent.duration.span as _
import scala.scalajs.js.JSConverters.*

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
