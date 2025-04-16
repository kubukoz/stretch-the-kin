package com.kubukoz.stretchthekin

import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import org.scalajs.dom.AudioContext

import scala.concurrent.duration.*

object Sounds {

  def playNormal: Resource[IO, Unit] = play(semitones = 0)

  def playEnding: Resource[IO, Unit] = play(semitones = 7)

  def playFinished = List(
    play(0) -> 200.millis,
    play(4) -> 200.millis,
    play(7) -> 200.millis,
    play(12) -> 400.millis,
  ).traverse_ { (action, duration) =>
    action.surround(IO.sleep(duration))
  }

  def play(semitones: Int) = Resource
    .make(IO(new AudioContext())) { audioCtx =>
      IO.fromPromise(IO(audioCtx.close()))
    }
    .evalMap { audioCtx =>
      IO {
        val mainGainNode = audioCtx.createGain()
        mainGainNode.gain.value = 0.3
        mainGainNode.connect(audioCtx.destination)

        val osc = audioCtx.createOscillator()
        osc.connect(mainGainNode)
        osc.`type` = "sine"

        osc.frequency.value = 440 * Math.pow(2, semitones / 12.0)
        osc.start()
      }
    }

}
