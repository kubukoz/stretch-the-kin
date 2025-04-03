package com.kubukoz.stretchthekin

import calico.html.io.*
import calico.html.io.given
import cats.effect.IO
import cats.effect.kernel.Resource
import com.kubukoz.stretchthekin.StepV2.Block
import fs2.concurrent.Signal
import fs2.dom.HtmlElement

import scala.concurrent.duration.*

object BlockComponents {

  def render(block: Block): Resource[IO, HtmlElement[IO]] =
    block match {
      case Block.Text(text, tag) =>
        tag match {
          case "h2" => h2(text)
        }
      case Block.Timer(initialDuration, decrementBy) =>
        CountdownComponent.render(
          countdownFrom = initialDuration,
          refreshRate = 1.second / 30,
          // todo: probably steal from the old one
          onFinished = IO.unit,
          // todo: probably steal from the old one
          isActive = Signal.constant(false),
        )
      case Block.Stacked(blocks) =>
        div(
          blocks.map(render)
        )
      case Block.Pages(pages, counted) =>
        div(
          // todo: this is completely wrong. Needs some sort of ref and an onFinished
          render(pages.head),
          // todo: implement counting (with the ref above)
          counted match {
            case true  => div(s"counted: 1/${pages.size}")
            case false => div("not counted")
          },
        )
    }

}

object ScreenComponentV2 {

  def render(step: StepV2): Resource[IO, HtmlElement[IO]] = div(
    h1(
      step.title + (if step.variants.nonEmpty then " | " + step.variants.mkString(" | ")
                    else
                      "")
    ),
    BlockComponents.render(step.content),
    styleAttr := "background-color: lightgreen; margin: 20px;",
  )

}
