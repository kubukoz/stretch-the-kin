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

  def render(block: Block, onFinished: IO[Unit]): Resource[IO, HtmlElement[IO]] =
    block match {
      case Block.Text(text, tag) =>
        tag match {
          case "h2" =>
            onFinished.background *>
              h2(text)
        }
      case Block.Timer(initialDuration, decrementBy) =>
        CountdownComponent.render(
          countdownFrom = initialDuration,
          refreshRate = 1.second / 30,
          onFinished = onFinished,
          isActive = Signal.constant(true),
        )
      case Block.Stacked(blocks) =>
        // All the steps need to be finished before the next one starts
        // so we count them concurrently.
        CountDownLatch[IO](blocks.size).toResource.flatMap { latch =>
          (latch.await *> onFinished).background *>
            div(
              blocks
                .zipWithIndex
                .map { (block, i) =>
                  render(
                    block,
                    onFinished =
                      latch.release *> IO.println(
                        s"finished step $i/${blocks.size}"
                      ),
                  )
                }
            )
        }
      case Block.Pages(pages, counted) =>
        SignallingRef[IO].of(0).toResource.flatMap { currentPageIndex =>
          div(
            currentPageIndex.map { i =>
              val page = pages(i)
              val isLast = i === pages.indices.last
              render(
                page,
                onFinished =
                  if isLast then onFinished
                  else
                    currentPageIndex.set(i + 1),
              )
            },
            counted match {
              // todo: also show +- buttons
              case true =>
                div(s"counted: ", currentPageIndex.map(_ + 1).map(_.show), s"/${pages.size}")
              case false => div("not counted")
            },
          )
        }
    }

}

object ScreenComponentV2 {

  def render(step: StepV2, onFinished: IO[Unit]): Resource[IO, HtmlElement[IO]] = div(
    h1(
      step.title + (if step.variants.nonEmpty then " | " + step.variants.mkString(" | ")
                    else
                      "")
    ),
    BlockComponents.render(step.content, onFinished),
    styleAttr := "margin: 20px;",
  )

}
