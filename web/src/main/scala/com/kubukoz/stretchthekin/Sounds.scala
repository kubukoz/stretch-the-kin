package com.kubukoz.stretchthekin

import cats.effect.IO
import cats.effect.kernel.Resource
import concurrent.duration.*
import org.scalajs.dom.AudioContext

object Sounds {

  def playShortSignal = Resource
    .make(IO(new AudioContext())) { audioCtx =>
      IO.fromPromise(IO(audioCtx.close()))
    }
    .use { audioCtx =>
      IO {
        val mainGainNode = audioCtx.createGain()
        mainGainNode.gain.value = 0.3
        mainGainNode.connect(audioCtx.destination)

        val osc = audioCtx.createOscillator()
        osc.connect(mainGainNode)
        osc.`type` = "sine"

        osc.frequency.value = 440 * Math.pow(2, 7 / 12.0)
        osc.start()

      }.andWait(500.millis)
    }

}
