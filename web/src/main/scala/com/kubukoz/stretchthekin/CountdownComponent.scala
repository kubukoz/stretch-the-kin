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

  private case class State(
    current: CurrentCounter,
    stored: FiniteDuration,
    initial: FiniteDuration,
  ) {

    private val cutoff = 0.seconds

    def ending: Boolean =
      if initial <= 2.seconds
      then total <= 1.seconds
      else if initial <= 5.seconds
      then total <= 2.seconds
      else
        total <= 5.seconds

    def finished: Boolean = total <= cutoff
    def total: FiniteDuration = (stored - current.runningTotal) max cutoff

    def paused: Boolean = current === CurrentCounter.Paused

  }

  def render(
    countdownFrom: FiniteDuration,
    refreshRate: FiniteDuration,
    onFinished: IO[Unit],
    isActive: Signal[IO, Boolean],
  ): Resource[IO, HtmlElement[IO]] = SignallingRef[IO]
    .of(State(current = CurrentCounter.Paused, stored = countdownFrom, initial = countdownFrom))
    .toResource
    .flatMap { state =>
      val total = state.map(_.total).changes
      val paused = state.map(_.paused).changes
      val current = state.lens(_.current)
      val finished = state.map(_.finished).changes

      val finishedOrInactive = (finished, isActive.map(!_)).mapN(_ || _)
      val finishedOrPaused = (finished, paused).mapN(_ || _)

      val ending =
        state
          .map(_.ending)
          .discrete
          .changes
          .filter(identity)
          .foreach(_ => Sounds.playShortSignal)
          .compile
          .drain
          .background
          .void

      val reset = state.update { s =>
        s.copy(
          current = CurrentCounter.Paused,
          stored = countdownFrom,
        )
      }

      val unpause =
        reset *> IO.realTime.flatMap { now =>
          state.update { s =>
            s.copy(
              current = CurrentCounter.Running(now, now)
            )
          }
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
          .pauseWhen(finishedOrPaused)
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
          .changes
          .filter(identity)
          .foreach(_ => onFinished)
          .compile
          .drain
          .background
          .void

      val unpauseExternal =
        isActive
          .discrete
          .changes
          .foreach {
            case true  => unpause
            case false => reset
          }
          .compile
          .drain
          .background
          .void

      div(
        p(
          "Remaining: ",
          total.map { e =>
            s"${e.toSeconds}.${(e.toMillis % 1000) / 10}s"
          },
        ),
        button.withSelf { self =>
          (
            disabled <-- finishedOrInactive,
            paused.map {
              case true  => "Resume"
              case false => "Pause"
            },
            onClick(switch),
            isActive
              .discrete
              .changes
              .filter(identity)
              .foreach(_ => IO.cede *> self.focus)
              .compile
              .drain
              .background
              .void,
          )
        },
        button(
          "Reset",
          onClick(reset),
          disabled <-- isActive.map(!_),
        ),
        updateState,
        notifyFinished,
        unpauseExternal,
        ending,
      )
    }

}
