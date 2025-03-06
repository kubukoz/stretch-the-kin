import calico.IOWebApp
import calico.html.io.*
import calico.html.io.given
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.syntax.all.*
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import language.experimental.namedTuples
import monocle.Focus

import scala.concurrent.duration.*

object App extends IOWebApp {

  import monocle.syntax.all.*

  extension [A](sigref: SignallingRef[IO, A]) {

    inline def lens[B](inline f: Focus.KeywordContext ?=> A => B): SignallingRef[IO, B] =
      SignallingRef.lens(sigref)(
        _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].get,
        _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].replace,
      )

    inline def sig: Signal[IO, A] = sigref
  }

  enum Step {
    case CARS
    case PassiveStretch(duration: FiniteDuration)
    case PAILS(duration: FiniteDuration)
    case RAILS(duration: FiniteDuration)
    case Liftoffs(number: Int)
  }

  case class Session(steps: List[Step])

  val sesshn = Session(
    List(
      Step.CARS,
      Step.PassiveStretch(FiniteDuration(30, "seconds")),
      Step.PAILS(FiniteDuration(30, "seconds")),
      Step.RAILS(FiniteDuration(30, "seconds")),
      Step.Liftoffs(10),
    )
  )

  enum CurrentCounter {
    case Paused
    case Running(since: FiniteDuration, now: FiniteDuration)
  }

  object CurrentCounter {

    extension (r: Running) {
      def runningTotal: FiniteDuration = r.now - r.since
    }

  }

  case class State(current: CurrentCounter, stored: FiniteDuration) {

    def total: FiniteDuration =
      stored + current.match {
        case CurrentCounter.Paused     => 0.seconds
        case r: CurrentCounter.Running => r.runningTotal
      }

    def paused: Boolean = current == CurrentCounter.Paused

  }

  def render: Resource[IO, HtmlElement[IO]] = SignallingRef[IO]
    .of(State(current = CurrentCounter.Paused, stored = 0.seconds))
    .toResource
    .flatMap { state =>
      val total = state.map(_.total)
      val paused = state.map(_.paused)
      val current = state.lens(_.current)

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
                stored = s.stored + r.runningTotal,
              )
          }
        }
      }

      val updateRate = 1.second / 60

      val updateState =
        fs2
          .Stream
          .fixedDelay[IO](updateRate)
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

      div(
        "Current time: ",
        total.map { e =>
          s"${e.toSeconds}:${e.toMillis % 1000}"
        },
        button(
          paused.map {
            case true  => "Resume"
            case false => "Pause"
          },
          onClick(switch),
        ),
        updateState,
      )
    }

}
