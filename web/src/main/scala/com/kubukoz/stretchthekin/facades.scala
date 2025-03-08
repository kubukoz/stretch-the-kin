package com.kubukoz.stretchthekin

import scalajs.js

import scala.scalajs.js.annotation.JSGlobal

@js.native
@JSGlobal
class SpeechSynthesisUtterance(message: String) extends js.Object {
  var voice: SpeechSynthesisVoice = js.native
  var pitch: Double = js.native
  var rate: Double = js.native
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
