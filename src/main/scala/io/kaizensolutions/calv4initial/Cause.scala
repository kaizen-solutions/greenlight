package io.kaizensolutions.calv4initial

import io.kaizensolutions.calv4initial.Cause.pruneCombine

sealed trait Cause[+A] extends Serializable with Product { self =>
  def map[B](f: A => B): Cause[B] = self match {
    case Cause.Single(a)               => Cause.single(f(a))
    case Cause.Both(a, b)              => Cause.both(a.map(f), b.map(f))
    case Cause.Then(first, second)     => Cause.andThen(first.map(f), second.map(f))
    case Cause.Fallback(first, second) => Cause.fallback(first.map(f), second.map(f))
    case Cause.Empty                   => Cause.Empty
  }

  def prune: Cause[A] = self match {
    case s @ Cause.Single(_) =>
      s

    case Cause.Both(l, r) =>
      pruneCombine(l, r)(Cause.Both(_, _))

    case Cause.Then(first, second) =>
      pruneCombine(first, second)(Cause.Then(_, _))

    case Cause.Fallback(first, second) =>
      pruneCombine(first, second)(Cause.Fallback(_, _))

    case e @ Cause.Empty =>
      e
  }
}

object Cause {
  case class Single[A](a: A)                                extends Cause[A]
  case class Both[A](a: Cause[A], b: Cause[A])              extends Cause[A]
  case class Then[A](first: Cause[A], second: Cause[A])     extends Cause[A]
  case class Fallback[A](first: Cause[A], second: Cause[A]) extends Cause[A]
  case object Empty                                         extends Cause[Nothing]

  def empty[A]: Cause[A]                              = Empty
  def single[A](a: A): Cause[A]                       = Single(a)
  def both[A](a: Cause[A], b: Cause[A]): Cause[A]     = Both(a, b)
  def andThen[A](a: Cause[A], b: Cause[A]): Cause[A]  = Then(a, b)
  def fallback[A](a: Cause[A], b: Cause[A]): Cause[A] = Fallback(a, b)

  def causeBothCombine[A]: Combine[Cause[A]]     = (a1: Cause[A], a2: Cause[A]) => both(a1, a2)
  def causeThenCombine[A]: Combine[Cause[A]]     = (a1: Cause[A], a2: Cause[A]) => andThen(a1, a2)
  def causeFallbackCombine[A]: Combine[Cause[A]] = (a1: Cause[A], a2: Cause[A]) => fallback(a1, a2)

  private def pruneCombine[A](left: Cause[A], right: Cause[A])(combiner: (Cause[A], Cause[A]) => Cause[A]) =
    (left.prune, right.prune) match {
      case (Cause.Empty, Cause.Empty) => Cause.Empty
      case (l, Cause.Empty)           => l
      case (Cause.Empty, r)           => r
      case (l, r)                     => combiner(l, r)
    }
}
