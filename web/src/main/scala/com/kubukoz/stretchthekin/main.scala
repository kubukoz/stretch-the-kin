package com.kubukoz.stretchthekin

import calico.IOWebApp
import calico.html.io.*
import calico.html.io.given
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.kernel.Monoid
import cats.syntax.all.*
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import io.circe.*
import io.circe.syntax.*
import monocle.Focus
import monocle.syntax.all.*

import scala.concurrent.duration.{span as _, *}

extension [A](sigref: SignallingRef[IO, A]) {

  inline def lens[B](inline f: Focus.KeywordContext ?=> A => B): SignallingRef[IO, B] =
    SignallingRef.lens(sigref)(
      _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].get,
      _.focus(f).asInstanceOf[monocle.AppliedLens[A, B]].replace,
    )

  inline def sig: Signal[IO, A] = sigref
}

object App extends IOWebApp {

  def render: Resource[IO, HtmlElement[IO]] = div(
    s"total time: ${Step.toScreens(Session.kneeIrEr45minute).map(_.time).flatten.combineAll.toMinutes}mins",
    ul(
      Step.toScreens(Session.kneeIrEr45minute).map(s => li(renderScreen(s)))
    ),
    Step.toInstructions(Session.kneeIrEr45minute).map(p(_)),
  )

  def renderScreen(s: Screen) = div(
    p(s.action + {
      if s.variants.isEmpty then ""
      else
        s" (${s.variants.mkString(", ")})"
    }),
    s.time.fold(span(""))(d => p(s"Time: ${d.toSeconds} seconds")),
    s.reps.fold(span(""))(n => p(s"Reps: $n")),
  )

  given Encoder[FiniteDuration] = _.toString.asJson

  case class Screen(
    action: String,
    time: Option[FiniteDuration],
    reps: Option[Int],
    variants: List[String],
  ) derives Encoder.AsObject

  enum Step derives Encoder.AsObject {
    case Empty
    case Single(action: String, timed: Option[FiniteDuration])
    case Many(steps: List[Step])
    case Sets(step: Step, n: Int, between: Step)
    case WithVariants(step: Step, variants: List[String])
    case Reps(step: Step, n: Int)

    def reps(n: Int): Step = Reps(this, n)
    def sets(n: Int, between: Step): Step = Sets(this, n, between)

    // repeats this step for each side (left, right)
    def leftRight: Step = WithVariants(this, List("Left side", "Right side"))

    // repeats this step for IR/ER
    def internalExternal: Step = WithVariants(this, List("Internal rotation", "External rotation"))

    def combine(another: Step): Step =
      (this, another) match {
        case (Empty, b)         => b
        case (a, Empty)         => a
        case (Many(a), Many(b)) => Many(a ++ b)
        case (Many(a), b)       => Many(a :+ b)
        case (a, Many(b))       => Many(a +: b)
        case _                  => Many(List(this, another))
      }

  }

  object Step {
    def steps(steps: Step*): Step = steps.toList.combineAll
    def step(action: String): Step.Single = Step.Single(action, timed = None)

    extension (s: Step.Single) {
      def timed(d: FiniteDuration): Step = s.copy(timed = d.some)
    }

    given Monoid[Step] with {
      def empty: Step = Empty
      def combine(x: Step, y: Step): Step = x.combine(y)
    }

    def toScreens(step: Step): List[Screen] = {
      def go(step: Step, reps: Option[Int], variants: List[String]): List[Screen] = {
        val recurse = go(_, reps, variants)

        step match {
          case Empty                  => Nil
          case Many(steps)            => steps.flatMap(recurse)
          case Single(action, timed)  => Screen(action, timed, reps, variants) :: Nil
          case Reps(step, n)          => go(step, Some(reps.getOrElse(1) * n), variants)
          case Sets(step, n, between) => recurse(List.fill(n)(step).intercalate(between))
          case WithVariants(step, vs) =>
            vs.flatMap { variant =>
              go(step, reps, variant :: variants)
            }
        }
      }

      go(step, reps = None, variants = Nil)
    }

    def toInstructions(step: Step): List[Resource[IO, HtmlElement[IO]]] =
      step match {
        case Empty       => Nil
        case Many(steps) => steps.flatMap(toInstructions)
        case Sets(step, n, between) =>
          List(
            p(s"$n sets of:"),
            ul(
              toInstructions(step).map(li(_)),
              li("Between sets:"),
              toInstructions(between),
            ),
          )
        case WithVariants(step, variants) =>
          List(
            p("For each variant: " + variants.mkString(", ")),
            ul(
              toInstructions(step).map(li(_))
            ),
          )

        case Reps(step, n) =>
          List(
            p(s"$n reps of:"),
            ul(
              toInstructions(step).map(li(_))
            ),
          )
        case Single(action, timed) =>
          List(
            p(action + timed.fold("")(d => s" for ${d.toSeconds} seconds"))
          )
      }

  }

  object Session {

    import Step.*

    val kneeIrEr45minute = steps(
      step("Knee CARs").reps(8).leftRight,
      step("Capsular knee CARs").reps(10).leftRight,
      steps(
        step("Get into position: 90-90 for internal rotation on the back"),
        step("Passive stretch").timed(2.minutes),
        steps(
          step("PAILs - begin slowly").timed(20.seconds),
          step("RAILs - switch instantly").timed(15.seconds),
          step("Slowly release").timed(10.seconds),
        ).sets(2, between = step("Passive stretch").timed(45.seconds)),
        step("Passive stretch").timed(30.seconds),
        step("Back out 10%").timed(10.seconds),
        steps(
          step("Lift up").timed(2.seconds),
          step("Lift down").timed(2.seconds),
        ).reps(9),
        step("Lift up and hold").timed(15.seconds),
        step("Slowly release").timed(10.seconds),
      ).leftRight.internalExternal,
      step("Knee CARs").reps(6).leftRight,
      step("Capsular knee CARs").reps(10).leftRight,
    )

  }

}
