package chapter4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(v) => Right(f(v))
    case Left(e) => Left(e)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Right(v) => b.map(f(v, _))
    case Left(e) => Left(e)
  }

  //  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
  //    case Nil => Right(Nil)
  //    case Right(x) :: xs => Right(x :: sequence(xs))
  //    case Left(x) :: xs
  //  }

  //  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]]

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

