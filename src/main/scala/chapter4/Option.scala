package chapter4

sealed trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case None => None
    case Some(v) => Some(f(v))
  }
  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case None => None
    case Some(v) => f(v)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(v) => v
  }
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case None => ob
    case _ => None
  }
  def filter(f: A => Boolean): MyOption[A] = this match {
    case Some(v) if f(v) => Some(v)
    case _ => None
  }
}

case object None extends MyOption[Nothing]
case class Some[A](value: A) extends MyOption[A]

object MyOption {
  def apply[A](v: A): MyOption[A] = if (v == null) None else Some(v)
}

object optionMain extends App {
  println(MyOption(4).map(_ * 5))
}
