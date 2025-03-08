package com.kubukoz.stretchthekin

import calico.html.io.*
import calico.html.io.given
import cats.derived.*
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import monocle.syntax.all.*

import scala.concurrent.duration.*

object CountdownComponent {

  private enum CurrentCounter derives Eq {
    case Paused
    case Running(since: FiniteDuration, now: FiniteDuration)

    def runningTotal: FiniteDuration =
      this match {
        case Running(since, now) => now - since
        case Paused              => 0.seconds
      }

  }

  private case class State(current: CurrentCounter, stored: FiniteDuration) {

    private val cutoff = 0.seconds

    def finished: Boolean = total <= cutoff
    def total: FiniteDuration = (stored - current.runningTotal) max cutoff

    def paused: Boolean = current === CurrentCounter.Paused

  }

  def render(
    countdownFrom: FiniteDuration,
    refreshRate: FiniteDuration,
    onFinished: IO[Unit],
    unpauseWhen: Signal[IO, Boolean],
  ): Resource[IO, HtmlElement[IO]] = SignallingRef[IO]
    .of(State(current = CurrentCounter.Paused, stored = countdownFrom))
    .toResource
    .flatMap { state =>
      val total = state.map(_.total)
      val paused = state.map(_.paused)
      val current = state.lens(_.current)
      val finished = state.map(_.finished)

      val unpause = IO.realTime.flatMap { now =>
        state.update { s =>
          s.copy(
            current = CurrentCounter.Running(now, now)
          )
        }
      }

      val reset = state.update { s =>
        s.copy(
          current = CurrentCounter.Paused,
          stored = countdownFrom,
        )
      }

      val switch = IO.realTime.flatMap { now =>
        state.update { s =>
          s.current.match {
            case CurrentCounter.Paused =>
              s.copy(
                current = CurrentCounter.Running(now, now)
              )

            case r: CurrentCounter.Running =>
              s.copy(
                current = CurrentCounter.Paused,
                stored = s.total,
              )
          }
        }
      }

      val updateState =
        fs2
          .Stream
          .fixedDelay[IO](refreshRate)
          .pauseWhen(finished)
          .foreach { _ =>
            IO.realTime.flatMap { now =>
              current.update {
                case r: CurrentCounter.Running => r.copy(now = now)
                case c                         => c
              }
            }
          }
          .compile
          .drain
          .background
          .void

      val notifyFinished =
        finished
          .discrete
          .filter(identity)
          .foreach(_ => onFinished)
          .compile
          .drain
          .background
          .void

      val unpauseExternal =
        unpauseWhen
          .discrete
          .filter(identity)
          .foreach(_ => unpause)
          .compile
          .drain
          .background
          .void

      div(
        "Remaining: ",
        total.map { e =>
          s"${e.toSeconds}.${(e.toMillis % 1000) / 10}s"
        },
        button(
          disabled <-- finished,
          paused.map {
            case true  => "Resume"
            case false => "Pause"
          },
          onClick(switch),
        ),
        button(
          "Reset",
          onClick(reset),
        ),
        updateState,
        notifyFinished,
        unpauseExternal,
      )
    }

}
