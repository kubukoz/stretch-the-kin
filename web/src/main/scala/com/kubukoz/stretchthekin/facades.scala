package com.kubukoz.stretchthekin

import cats.effect.IO
import scalajs.js

import scala.annotation.nowarn
import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal
@nowarn("msg=unused")
class SpeechSynthesisUtterance(message: String) extends js.Object {
  var voice: js.UndefOr[SpeechSynthesisVoice] = js.native
  var pitch: Double = js.native
  var rate: Double = js.native
  var onend: js.Function0[Unit] = js.native
}

@js.native
@JSGlobal
class SpeechSynthesisVoice extends js.Object {
  var name: String = js.native
  var lang: String = js.native
}

@js.native
@JSGlobal
class SpeechSynthesis extends js.Object {
  def speak(utterance: SpeechSynthesisUtterance): Unit = js.native
  def getVoices(): js.Array[SpeechSynthesisVoice] = js.native
}

trait SpeechSynthesisIO {
  def speak(utterance: SpeechSynthesisUtterance): IO[Unit]
  def getVoices: IO[List[SpeechSynthesisVoice]]
}

object SpeechSynthesisIO extends SpeechSynthesisIO {

  private val synth = org
    .scalajs
    .dom
    .window
    .asInstanceOf[js.Dynamic]
    .speechSynthesis
    .asInstanceOf[SpeechSynthesis]

  def getVoices: IO[List[SpeechSynthesisVoice]] = IO(synth.getVoices().toList)

  def speak(utterance: SpeechSynthesisUtterance): IO[Unit] = IO.async_[Unit] { cb =>
    utterance.onend = () => cb(Right(()))
    synth.speak(utterance)
  }

}
