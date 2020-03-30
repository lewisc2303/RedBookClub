package chapter5

import chapter5.Stream.unfold

sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, tail) => h.apply() :: tail.apply().toList
  }

  def append[B >: A](that: => Stream[B]): Stream[B] = this match {
    case Empty => that
    case Cons(h, t) => Cons(h, () => t.apply.append(that))
  }

  def take(n: Int): Stream[A] = {
    def loop(stream: Stream[A], n: Int, newStream: Stream[A]): Stream[A] = stream match {
      case Empty => Empty
      case _ if n == 0 => newStream
      case Cons(h, t) => loop(t.apply(), n - 1, newStream.append(Cons(h, () => Empty)))
    }

    loop(this, n, Empty)
  }

  def drop(n: Int): Stream[A] = {
    def loop(stream: Stream[A], n: Int): Stream[A] = stream match {
      case Empty => Empty
      case _ if n == 0 => stream
      case Cons(_, t) => loop(t.apply(), n - 1)
    }

    loop(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def loop(stream: Stream[A], newStream: Stream[A]): Stream[A] = stream match {
      case Cons(h, t) if p(h.apply()) => loop(t.apply(), newStream.append(Cons(h, () => Empty)))
      case _ => newStream
    }

    loop(this, Empty)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forall(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] = this.foldRight(Empty: Stream[A]) {
    (a, b) => if (p(a)) Cons(() => a, () => b) else Empty
  }

  //  def headOption: Option[A] = this.foldRight(None: Option[A])((a, b) => if (!a.isInstanceOf[A]) b else Some(a))
  def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] = this.foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))

  def filter(p: A => Boolean): Stream[A] = this.foldRight(Empty: Stream[A]) { (a, b) =>
    if (p(a)) Cons(() => a, () => b) else b
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {  //looked up
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(_, _), 0)  => None
    case (Cons(h, t), num) => Some(h(), (t(), num - 1 ))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B](that: Stream[B]): Stream[(A, B)] = unfold(this, that) {
    case (Cons(h1, t1),Cons(h2, t2)) => Some((h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B, C](that: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =  //looked up
    Stream.unfold((this, that)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), Empty))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (Empty -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(a: Int): Stream[Int] = cons(a, constant(a + 1))

  def fibs: Stream[Int] = {
    def loop(prev: Int, now: Int): Stream[Int] = {
      cons(prev, loop(now, prev + now))
    }

    loop(0, 1)
  }

  //infinite streams and corecursion

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((value, next)) => cons(value, unfold(next)(f))
    case None => Empty
  }

  def fibsWithUnfold: Stream[Int] = unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

  def fromWithUnfold(a: Int): Stream[Int] = unfold(a) { a => Some(a, a + 1) }

  def oneWithUnfold: Stream[Int] = unfold(1) { _ => Some(1, 1) }
}


object main extends App {
  println(Stream(1, 2, 3, 4, 5).toList)

  println(Stream(1, 2, 3, 4, 5).take(3).toList)

  println(Stream(1, 2, 3, 4, 5).drop(3).toList)

  println(Stream(1, 2, 3, 4, 5).takeWhile(_ < 3).toList)

  println(Stream(1, 2, 3, 4, 5).forall(_ < 3))

  println(Stream(1, 2, 3, 4, 5).takeWhile2(_ > 3).toList) //not take while

  println(Stream(1, 2, 3, 4, 5).headOption)
  println(Empty.headOption)

  println(Stream(1, 2, 3, 4, 5).map(_ * 2).toList)

  println(Stream(1, 2, 3, 4, 5).filter(_ > 3).toList)

  println(Stream.fibs.take(10).toList)

  println(Stream.fibsWithUnfold.take(10).toList)

  println(Stream(1,2,3,4,5).takeViaUnfold(2).toList)

  println(Stream(1,2,3,4,1).takeWhileViaUnfold(_<3).toList)

  println(Stream(1,2).zipWith(Stream("one", "two")).toList)




}
