package io.kaizensolutions.calvin3initial

trait Combine[A] {
  def combine(a1: A, a2: A): A
}

object Combine extends LowPriorityCombine {}

trait LowPriorityCombine {
  implicit def combineSeq[A]: Combine[Seq[A]] =
    (a1: Seq[A], a2: Seq[A]) => a1 ++ a2
}
