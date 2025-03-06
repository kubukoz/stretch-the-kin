import calico.IOWebApp
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
import language.experimental.namedTuples
import monocle.Focus
import monocle.syntax.all.*

import scala.concurrent.duration.*

extension [A](sigref: SignallingRef[IO, A]) {

  inline def lens[B](inline f: Focus.KeywordContext ?=> A => B): SignallingRef[IO, B] =
    SignallingRef.lens(sigref)(
      _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].get,
      _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].replace,
    )

  inline def sig: Signal[IO, A] = sigref
}

object App extends IOWebApp {

  def render: Resource[IO, HtmlElement[IO]] = SignallingRef[IO].of(0).toResource.flatMap {
    countdownActiveI =>
      val countdowns =
        List.fill(10) {
          CountdownComponent
            .render(
              countdownFrom = 1.seconds,
              refreshRate = 1.second / 60,
              onFinished = IO.println("finished") *> countdownActiveI.update(i => (i + 1) % 10),
            )
        }

      div(
        "Counter ",
        countdownActiveI.map(_.show),
        ":",
        countdowns.liftN(countdownActiveI.sig),
      )

  }

  // enum Step {
  //   case CARS
  //   case PassiveStretch(duration: FiniteDuration)
  //   case PAILS(duration: FiniteDuration)
  //   case RAILS(duration: FiniteDuration)
  //   case Liftoffs(number: Int)
  // }

  // case class Session(steps: List[Step])

  // val sesshn = Session(
  //   List(
  //     Step.CARS,
  //     Step.PassiveStretch(FiniteDuration(30, "seconds")),
  //     Step.PAILS(FiniteDuration(30, "seconds")),
  //     Step.RAILS(FiniteDuration(30, "seconds")),
  //     Step.Liftoffs(10),
  //   )
  // )

}

object CountdownComponent {

  enum CurrentCounter derives Eq {
    case Paused
    case Running(since: FiniteDuration, now: FiniteDuration)

    def runningTotal: FiniteDuration =
      this match {
        case Running(since, now) => now - since
        case Paused              => 0.seconds
      }

  }

  case class State(current: CurrentCounter, stored: FiniteDuration) {

    private val cutoff = 0.seconds

    def finished: Boolean = total <= cutoff
    def total: FiniteDuration = (stored - current.runningTotal) max cutoff

    def paused: Boolean = current === CurrentCounter.Paused

  }

  def render(
    countdownFrom: FiniteDuration,
    refreshRate: FiniteDuration,
    onFinished: IO[Unit],
  ): Resource[IO, HtmlElement[IO]] = SignallingRef[IO]
    .of(State(current = CurrentCounter.Paused, stored = countdownFrom))
    .toResource
    .flatMap { state =>
      val total = state.map(_.total)
      val paused = state.map(_.paused)
      val current = state.lens(_.current)
      val finished = state.map(_.finished)

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
          .interruptWhen(finished)
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

      div(
        "Current time: ",
        total.map { e =>
          s"${e.toSeconds}:${e.toMillis % 1000}"
        },
        button(
          disabled <-- finished,
          paused.map {
            case true  => "Resume"
            case false => "Pause"
          },
          onClick(switch),
        ),
        updateState,
        notifyFinished,
      )
    }

}
