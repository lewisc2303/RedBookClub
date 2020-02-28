package chapter3

import scala.annotation.tailrec

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def apply[A](as: A*): MyList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, tail) => x + sum(tail)
  }

  def product(ints: MyList[Int]): Int = ints match {
    case Nil => 1
    case Cons(0, _) => 0
    case Cons(x, tail) => x * product(tail)
  }

  def tail[A](list: MyList[A]): MyList[A] = list match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def setHead[A](list: MyList[A], newHead: A): MyList[A] = list match {
    case Nil => Nil
    case Cons(_, tail) => Cons(newHead, tail)
  }

  def drop[A](list: MyList[A], n: Int): MyList[A] = list match {
    case Nil => Nil
    case Cons(x, tail) if n == 0 => Cons(x, tail)
    case Cons(x, tail) => drop(tail, n - 1)
  }

  def dropWhile[A](list: MyList[A], predicate: A => Boolean): MyList[A] = list match {
    case Nil => Nil
    case Cons(x, tail) if predicate(x) => dropWhile(tail, predicate)
    case x => x
  }

  def betterDropWhile[A](list: MyList[A])(predicate: A => Boolean): MyList[A] = list match {
    case Nil => Nil
    case Cons(x, tail) if predicate(x) => betterDropWhile(tail)(predicate)
    case x => x
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t,  a2))
  }

  def init[A](list: MyList[A]): MyList[A] = {
    @tailrec
    def loop(list: MyList[A], acc: MyList[A]): MyList[A] = list match {
      case Nil => Nil
      case Cons(_, Nil) => acc
      case Cons(x, tail) => loop(tail, append(acc, Cons(x, Nil)))
    }
    loop(list, Nil)
  }

  //not stack safe because functions are recursively created until the list has been
  // traversed but not evaluated until then either so can cause stack overflow
  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def safeFoldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldRight(xs, f(x, z))(f)
  }

  def length[A](list: MyList[A]) : Int = list match  {
    case Nil => 0
    case Cons(_, tail) =>  1 + length(tail)
  }

  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }

  def sum2(ints: MyList[Int]): Int = foldLeft(ints, 0)(_+_)

  def product2(ints: MyList[Int]): Int = foldLeft(ints, 1)(_*_)

  def length2[A](list: MyList[A]): Int = foldLeft(list, 0)((acc, _) => acc + 1)

  def reverse[A](list: MyList[A]): MyList[A] = foldLeft(list, Nil: MyList[A])((acc, x) => Cons(x, acc))

  //implement append in terms of fold
  def append2[A](list1: MyList[A], list2: MyList[A]): MyList[A] = ???

  //implement foldLeft in terms of foldRight

  //write a function that concatenates a list of lists
  def flattenList[A](listOfLists: MyList[MyList[A]]): MyList[A] = foldLeft(listOfLists, Nil: MyList[A])((acc, x) => append(acc, x))

  def listToString(list: MyList[Double]): MyList[String] = foldLeft(list, Nil: MyList[String])((acc, x)  => Cons(x.toString, acc))

  def map[A, B](list: MyList[A], f: A => B): MyList[B] = list match {
    case Nil => Nil
    case Cons(x, tail) => Cons(f(x), map(tail, f))
  }


}

object main extends App {
  import MyList._

  println(reverse(MyList(1,2,3,4)))

  println(flattenList(MyList(MyList(1,2,3), MyList(4,5,6))))
}


