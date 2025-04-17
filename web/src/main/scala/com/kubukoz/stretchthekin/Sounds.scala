package com.kubukoz.stretchthekin

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import org.scalajs.dom.AudioContext

import scala.concurrent.duration.*

opaque type Sounds = AudioContext

object Sounds {

  def make: Resource[IO, Sounds] =
    Resource
      // This is actually heavy, and has caused performance problems on mobile
      .make(IO(new AudioContext())) { audioCtx =>
        IO.fromPromise(IO(audioCtx.close()))
      }

  def playNormal(
    using Sounds
  ): Resource[IO, Unit] = play(semitones = 0)

  def playEnding(
    using Sounds
  ): Resource[IO, Unit] = play(semitones = 7)

  def playFinished(
    using Sounds
  ) = List(
    play(0) -> 200.millis,
    play(4) -> 200.millis,
    play(7) -> 200.millis,
    play(12) -> 400.millis,
  ).traverse_ { (action, duration) =>
    action.surround(IO.sleep(duration))
  }

  def play(
    semitones: Int
  )(
    using Sounds
  ): Resource[IO, Unit] =
    Resource.make {
      IO {
        val audioCtx = summon[Sounds]

        val mainGainNode = audioCtx.createGain()
        mainGainNode.gain.value = 0.3
        mainGainNode.connect(audioCtx.destination)

        val osc = audioCtx.createOscillator()
        osc.connect(mainGainNode)
        osc.`type` = "sine"

        osc.frequency.value = 440 * Math.pow(2, semitones / 12.0)
        osc.start()
        osc
      }
    } { osc =>
      IO(osc.stop())
    }.void

}
