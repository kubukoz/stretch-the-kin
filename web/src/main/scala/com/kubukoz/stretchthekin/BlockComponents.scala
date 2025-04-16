package com.kubukoz.stretchthekin

import calico.html.io.*
import calico.html.io.given
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.std.CountDownLatch
import cats.syntax.all.*
import com.kubukoz.stretchthekin.StepV2.Block
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement

import scala.concurrent.duration.*

object BlockComponents {

  def render(
    block: Block,
    onFinished: IO[Unit],
  )(
    using Speaker
  ): Resource[IO, HtmlElement[IO]] =
    block match {
      case Block.Text(text, tag) =>
        val announce =
          Sounds.playNormal.surround(IO.sleep(200.millis)).background *>
            Speaker.speak(text).background

        val render = tag.match { case "h2" => h2(text) }

        announce *>
          render <*
          onFinished.background

      case Block.Timer(initialDuration, playToneWhenEnding) =>
        Sounds.playNormal.surround(IO.sleep(200.millis)).background *>
          CountdownComponent.render(
            countdownFrom = initialDuration,
            refreshRate = 1.second / 30,
            onFinished = onFinished,
            isActive = Signal.constant(true),
            shouldPlayToneWhenEnding = playToneWhenEnding,
          )
      case Block.Parallel(blocks) =>
        // All the steps need to be finished before the next one starts
        // so we count them concurrently.
        CountDownLatch[IO](blocks.size).toResource.flatMap { latch =>
          IO.println("rendering these sweet blocks: " + blocks).toResource *>
            (latch.await *> onFinished).background *>
            div(
              blocks
                .zipWithIndex
                .map { (block, i) =>
                  render(
                    block,
                    onFinished =
                      latch.release *> IO.println(
                        s"finished step $i/${blocks.size}: ${block}"
                      ),
                  )
                }
            )
        }
      case Block.Sequential(pages, counted) =>
        SignallingRef[IO].of(0).toResource.flatMap { currentPageIndex =>
          val previous = currentPageIndex.update { i =>
            val isFirst = i === 0
            if isFirst then i
            else
              i - 1
          }

          val next = currentPageIndex.flatModify { i =>
            val isLast = i === pages.indices.last

            if isLast then (i, onFinished)
            else
              (i + 1, IO.unit)
          }

          div(
            currentPageIndex.changes.map { i =>
              render(
                pages(i),
                onFinished = next,
              )
            },
            counted match {
              case true =>
                div(
                  s"rep: ",
                  currentPageIndex.map(_ + 1).map(_.show),
                  s"/${pages.size}",
                  button(
                    "-",
                    disabled <-- currentPageIndex.map(_ === 0),
                    onClick(previous),
                  ),
                  button(
                    "+",
                    onClick(next),
                  ),
                ).some
              case false => none
            },
          )
        }
    }

}

object ScreenComponentV2 {

  def render(
    step: StepV2,
    onFinished: IO[Unit],
  )(
    using Speaker
  ): Resource[IO, HtmlElement[IO]] = {
    val speechText = (step.title +: step.variants).mkString(", ")

    Speaker.speak(speechText).background *>
      div(
        h1(
          step.title + (if step.variants.nonEmpty then " | " + step.variants.mkString(" | ")
                        else
                          "")
        ),
        BlockComponents.render(step.content, onFinished),
        styleAttr := "margin: 20px;",
      )
  }

}
