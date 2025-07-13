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
    activeOnLaunch: Boolean,
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
    shouldPlayToneWhenEnding: Boolean,
  )(
    using Sounds
  ): Resource[IO, HtmlElement[IO]] = isActive
    .get
    .flatMap { active =>
      SignallingRef[IO]
        .of(
          State(
            current = CurrentCounter.Paused,
            stored = countdownFrom,
            initial = countdownFrom,
            activeOnLaunch = active,
          )
        )
    }
    .toResource
    .flatMap { state =>
      val total = state.map(_.total).changes
      val paused = state.map(_.paused).changes
      val current = state.lens(_.current)
      val finished = state.map(_.finished).changes

      val finishedOrInactive = (finished, isActive.map(!_)).mapN(_ || _)
      val finishedOrPaused = (finished, paused).mapN(_ || _)

      val playToneWhenEnding =
        state
          .map(_.ending && shouldPlayToneWhenEnding)
          .discrete
          .changes
          .filter(identity)
          .foreach(_ => Sounds.playEnding.surround(IO.sleep(200.millis)))
          .compile
          .drain
          .background

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

            case _: CurrentCounter.Running =>
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

      val notifyFinished =
        finished
          .discrete
          .changes
          .filter(identity)
          .foreach(_ => onFinished)
          .compile
          .drain
          .background

      val unpauseExternal = isActive
        .getAndDiscreteUpdates
        .flatMap { (activeNow, activeDiscrete) =>
          // if the component is active on launch, we need to unpause it before we even render
          unpause.whenA(activeNow).toResource *>
            activeDiscrete
              .changes
              .foreach {
                case true  => unpause
                case false => reset
              }
              .compile
              .drain
              .background
        }

      updateState *>
        notifyFinished *>
        unpauseExternal *>
        playToneWhenEnding *>
        div(
          p(
            "Remaining: ",
            total.map { e =>
              s"${e.toSeconds}.${(e.toMillis % 1000) / 10}s"
            },
          ),
          button.withSelf { self =>
            val focusOnActivate =
              isActive
                .discrete
                .changes
                .filter(identity)
                .foreach(_ => IO.cede *> self.focus)
                .compile
                .drain
                .background

            focusOnActivate.as(
              disabled <-- finishedOrInactive,
              paused.map {
                case true  => "Resume"
                case false => "Pause"
              },
              onClick(switch),
            )
          },
          button(
            "Reset",
            onClick(reset),
            disabled <-- isActive.map(!_),
          ),
        )
    }

}
