package io.kaizensolutions.nigelv1

// Idea similar to https://github.com/zio/zio-prelude/pull/229
// see if their encoding is better
sealed trait Cause[+A] extends Serializable with Product { self =>
  def map[B](f: A => B): Cause[B] = self match {
    case Cause.Single(a)               => Cause.single(f(a))
    case Cause.Both(a, b)              => Cause.both(a.map(f), b.map(f))
    case Cause.Then(first, second)     => Cause.andThen(first.map(f), second.map(f))
    case Cause.Fallback(first, second) => Cause.fallback(first.map(f), second.map(f))
    case Cause.Empty                   => Cause.Empty
  }

  def foreach(f: A => Unit): Unit = self match {
    case Cause.Single(a)               => Cause.single(f(a))
    case Cause.Both(a, b)              => Cause.both(a.map(f), b.map(f))
    case Cause.Then(first, second)     => Cause.andThen(first.map(f), second.map(f))
    case Cause.Fallback(first, second) => Cause.fallback(first.map(f), second.map(f))
    case Cause.Empty                   => Cause.Empty
  }
}

object Cause {
  private final case class  Single[A](a: A)                                extends Cause[A]
  private final case class  Both[A](a: Cause[A], b: Cause[A])              extends Cause[A]
  private final case class  Then[A](first: Cause[A], second: Cause[A])     extends Cause[A]
  private final case class  Fallback[A](first: Cause[A], second: Cause[A]) extends Cause[A]
  private final case object Empty                                          extends Cause[Nothing]

  private def optimizeCause[A](c: Cause[A]): Cause[A] = c match {
    case Both(Empty, Empty)     => Empty
    case Single(Empty)          => Empty
    case Both(a, Empty)         => a
    case Both(Empty, b)         => b
    case Fallback(Empty, Empty) => Empty
    case Fallback(a, Empty)     => a
    case Fallback(Empty, b)     => b
    case _                      => c
  }

  def empty[A]: Cause[A]                              = Empty
  def single[A](a: A): Cause[A]                       = optimizeCause(Single(a))
  def both[A](a: Cause[A], b: Cause[A]): Cause[A]     = optimizeCause(Both(a, b))
  def andThen[A](a: Cause[A], b: Cause[A]): Cause[A]  = optimizeCause(Then(a, b))
  def fallback[A](a: Cause[A], b: Cause[A]): Cause[A] = optimizeCause(Fallback(a, b))
}
