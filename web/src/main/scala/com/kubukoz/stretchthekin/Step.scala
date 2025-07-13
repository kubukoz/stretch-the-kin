package com.kubukoz.stretchthekin

import cats.syntax.all.*
import io.circe.*

import scala.concurrent.duration.*
import scala.util.NotGiven

case class Step(
  title: String,
  content: Step.Block,
  variants: List[String] = Nil,
) derives Encoder.AsObject {
  def addVariants(variants: List[String]): Step = this.copy(variants = this.variants ++ variants)

  def fullTitle: String =
    this.title +
      (if this.variants.nonEmpty then " | " + this.variants.mkString(" | ")
       else
         "")

}

object Step {

  given [T <: String](
    using NotGiven[T =:= String]
  ): Encoder[T] = Json.fromString(_)

  enum Block derives Encoder.AsObject {

    def totalTime: FiniteDuration =
      this match {
        case Sequential(pages, _)      => pages.map(_.totalTime).combineAll
        case Parallel(blocks)          => blocks.map(_.totalTime).combineAll
        case Text(_, _)                => 0.seconds
        case Timer(initialDuration, _) => initialDuration
      }

    case Timer(
      initialDuration: FiniteDuration,
      playToneWhenEnding: Boolean,
    )

    // only one visible at a time. if counted, shows +/- buttons
    case Sequential(pages: List[Block], counted: Boolean)
    // all visible at once
    case Parallel(blocks: List[Block])
    // just a text node of some sort
    case Text(text: String, tag: "h2")
  }

  object Block {

    def subheadingTimed(text: String, time: FiniteDuration, playToneWhenEnding: Boolean): Block =
      Parallel(
        List(
          Text(text, "h2"),
          Timer(time, playToneWhenEnding),
        )
      )

    def pages(pages: Block*): Sequential = Sequential(pages.toList, counted = false)
    extension (pages: Sequential) def withCounting: Sequential = pages.copy(counted = true)

    def byReps(n: Int)(blockForRep: Int => Block): Block =
      pages(
        (1 to n).map(blockForRep).toList*
      ).withCounting

    def liftoffs(
      n: Int,
      repTime: FiniteDuration,
      finalHoldTime: FiniteDuration,
      finalReleaseTime: FiniteDuration,
    ): Block =
      byReps(n) { number =>
        val isLast = number == n

        if isLast then pages(
          subheadingTimed("Lift up and hold", repTime, playToneWhenEnding = true),
          subheadingTimed("HOLD", finalHoldTime, playToneWhenEnding = true),
          subheadingTimed("Slowly release", finalReleaseTime, playToneWhenEnding = true),
        )
        else
          pages(
            subheadingTimed("Lift up", repTime, playToneWhenEnding = false),
            subheadingTimed("Lift down", repTime, playToneWhenEnding = false),
          )
      }

    def cars(n: Int, timePerRep: FiniteDuration): Block =
      byReps(n)(_ => Timer(timePerRep, playToneWhenEnding = false))
  }

  def liftoffs(n: Int): Step = Step(
    title = "Liftoffs",
    content = Block.liftoffs(
      n = n,
      repTime = 2.seconds,
      finalHoldTime = 15.seconds,
      finalReleaseTime = 10.seconds,
    ),
  )

  val leftRight = List("Left side", "Right side")
  val clockwiseCounterclockwise = List("Clockwise", "Counterclockwise")
  val internalExternal = List("IR", "ER")

  extension (steps: List[Step]) {

    def matrix(axes: List[String]*): List[Step] = axes.toList.sequence.flatMap { variants =>
      steps.map(step => step.addVariants(variants))
    }

    def totalTime: FiniteDuration = steps.map(_.content.totalTime).combineAll

  }

  extension (step: Step) {
    def matrix(axes: List[String]*): List[Step] = List(step).matrix(axes*)
  }

  def cars(n: Int): List[Step] = Step(
    title = "CARs",
    content = Block.cars(
      n = n,
      timePerRep = 4.seconds,
    ),
  ).matrix(
    leftRight,
    clockwiseCounterclockwise,
  )

  def capsularCars(n: Int): List[Step] = Step(
    title = "Capsular CARs",
    content = Block.cars(
      n = n,
      timePerRep = 3.seconds,
    ),
  ).matrix(leftRight)

  def simpleTimed(text: String, subtext: String, time: FiniteDuration): Step = Step(
    title = text,
    content = Block.subheadingTimed(subtext, time, playToneWhenEnding = true),
  )

  def combineSteps(steps: (Step | List[Step])*): List[Step] = steps.toList.flatMap {
    case s: Step       => List(s)
    case l: List[Step] => l
  }

  def pailsRailsRound(): List[Step] = List(
    simpleTimed("PAILs", "Begin slowly, hold", 20.seconds),
    simpleTimed("RAILS", "Switch instantly, hold", 15.seconds),
    simpleTimed("Slow release", "Relax", 10.seconds),
  )

  def pailsRailsRounds(n: Int): List[Step] = (1 to n).toList.flatMap { roundNumber =>
    val isLast = roundNumber == n

    val passiveStretch = simpleTimed("Passive stretch", "relax", 45.seconds)

    if isLast then pailsRailsRound()
    else
      pailsRailsRound() :+ passiveStretch
  }

  val kneeIrEr45minute = combineSteps(
    simpleTimed("Prepare", "Sit down for knee CARs", 5.seconds),
    // warmup
    cars(8),
    capsularCars(10),

    // PAILS/RAILS
    combineSteps(
      simpleTimed(
        "Get into position",
        "90 90, internal in the back, external in the front",
        30.seconds,
      ),
      simpleTimed("Active stretch", "Find your end range position", 20.seconds),
      simpleTimed("Passive stretch", "Pull yourself to 10% outside your active ROM", 10.seconds),
      simpleTimed("Passive stretch", "Relax", 2.minutes),
      pailsRailsRounds(3),
      simpleTimed("Passive stretch", "Relax", 30.seconds),
      simpleTimed("Back out for liftoffs", "10%", 10.seconds),
      liftoffs(10),
    ).matrix(leftRight, internalExternal),

    // cooldown
    cars(6),
    capsularCars(10),
  )

}
