package io.kaizensolutions.calv4initial

object ValidatorTrace {
  final case class Trace[+E, +W](
    warnings: Cause[W] = Cause.empty,
    errors: Cause[E] = Cause.empty
  ) { self =>

    def combine[E1 >: E, W1 >: W](that: Trace[E1, W1])(
      combineE: (Cause[E1], Cause[E1]) => Cause[E1],
      combineW: (Cause[W1], Cause[W1]) => Cause[W1]
    ): Trace[E1, W1] =
      Trace(
        combineW(self.warnings, that.warnings).prune,
        combineE(self.errors, that.errors).prune
      )

    def andThen[E1 >: E, W1 >: W](that: Trace[E1, W1]): Trace[E1, W1] =
      combine(that)(Cause.andThen, Cause.andThen)

    def fallback[E1 >: E, W1 >: W](that: Trace[E1, W1]): Trace[E1, W1] =
      combine(that)(Cause.fallback, Cause.fallback)

    def both[E1 >: E, W1 >: W](that: Trace[E1, W1]): Trace[E1, W1] =
      combine(that)(Cause.both, Cause.both)
  }

  def fromResult[E, W](r: Result[E, W, _]): Trace[E, W] =
    r match {
      case Result.Success(w, _) =>
        Trace(w.fold(Cause.empty[W])(Cause.single[W]), Cause.empty)

      case Result.Error(w, e) =>
        Trace(w.fold(Cause.empty[W])(Cause.single[W]), Cause.single(e))
    }

  implicit class ValidatorTraceOps[I, E, W, A](in: Validator[I, E, W, A]) {
    def trace(i: I): Trace[E, W] = trace0(i, Trace())

    private def trace0(i: I, acc: Trace[E, W]): Trace[E, W] = in match {
      case Validator.Pure(underlying) =>
        acc.andThen(fromResult(underlying(i)))

      case Validator.MapInput(underlying, f) =>
        acc.andThen(fromResult(underlying.run(i)))

      case Validator.MapResult(underlying, f) =>
        acc.andThen(fromResult(underlying.run(i)))

      case mewi: Validator.MapErrorWithInput[I, E, e1, W, A] =>
        acc.andThen(
          fromResult(mewi.run(i).mapError(e => mewi.f(i, e)))
        )

      case mw: Validator.MapWarning[I, E, W, w, A] =>
        acc.andThen(
          fromResult(mw.run(i).mapWarning(mw.f))
        )

      case me: Validator.MapError[I, E, e, W, A] =>
        acc.andThen(
          fromResult(me.run(i).mapError(me.f))
        )

      case at: Validator.AndThen[I, E, W, A, b] =>
        val traceA = at.first.trace(i)
        val resA   = at.first.run(i)
        resA match {
          case Result.Success(_, result) =>
            traceA andThen at.second.trace(result)

          case Result.Error(_, _) =>
            traceA
        }

      case fm: Validator.FlatMap[I, E, W, A, b] =>
        val traceA = fm.underlying.trace(i)
        val resA   = fm.underlying.run(i)
        resA match {
          case Result.Success(_, result) =>
            val traceB = fm.f(result).trace(i)
            traceA andThen traceB

          case Result.Error(_, _) =>
            traceA
        }

      case zw: Validator.ZipWith[I, E, W, A, b, c] =>
        val traceA = zw.left.trace(i)
        val traceB = zw.right.trace(i)
        traceA both traceB

      case ew: Validator.EitherWith[I, E, W, A, b, c] =>
        val traceA = ew.first.trace(i)
        ew.first.run(i) match {
          case Result.Success(_, _) =>
            traceA

          case Result.Error(_, _) =>
            val traceB = ew.second.trace(i)
            traceA fallback traceB
        }
    }
  }
}
