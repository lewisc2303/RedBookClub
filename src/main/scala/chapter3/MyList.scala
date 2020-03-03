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
    case list if n == 0 => list
    case Cons(x, tail) => drop(tail, n - 1)
  }

  def take[A](list: MyList[A], n: Int): MyList[A] = {
    def loop[A](list: MyList[A], newList: MyList[A], n: Int): MyList[A] =
      list match {
        case Nil => Nil
        case _ if n == 0 => newList
        case Cons(x, tail) => loop(tail, append(newList, MyList(x)), n - 1)
      }
    loop(list, Nil, n)
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

  def filter[A, B](list: MyList[A], predicate: A => Boolean): MyList[A] = list match {
    case Nil => Nil
    case Cons(x, tail) if predicate(x) => Cons(x, filter(tail, predicate))
    case Cons(_, tail) => filter(tail, predicate)
  }

  def flatMap[A, B](list: MyList[A])(f: A => MyList[B]): MyList[B] = list match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  //implement flatmap using filter?
  def flatMapFilter[A, B](list: MyList[A], predicate: A => Boolean): MyList[A] = ???

  def sumLists(list1: MyList[Int], list2: MyList[Int]): MyList[Int] = (list1, list2) match {
    case (list1, list2) if length(list1) != length(list2) => throw new Error
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, sumLists(xs, ys))
  }

  def zipWith[A, B, C](list1: MyList[A], list2: MyList[B])(f: (A, B) => C ): MyList[C] = (list1, list2) match {
    case (list1, list2) if length(list1) != length(list2) => throw new Error
    case (Nil,  Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
  }

//  def createLists[A](list: MyList[A], n: Int): MyList[Int] = list match {
//    case Nil => Cons(0, Nil)
//    case Cons(_, tail) => Cons(n, createLists(tail, n - 1))
//  }
//
//  def hasSeqsequence[A](sup: MyList[A], sub: MyList[A]): MyList[MyList[A]] = {
//    val listLength = length(sup)
//
//    def loop[A](sup: MyList[A], n: Int, lists: MyList[MyList[A]]): MyList[MyList[A]] = sup match {
//      case _ if n == 0 => lists
//      case Nil => Nil
//      case Cons(_, xs) => loop(xs, n - 1, append(lists, map(reverse(createLists(sup, n)), (int: Int) =>  drop(sup, int))))
//    }
//    filter(loop(sup, listLength, Nil), (x: MyList[A]) => x == sub)
//  }

}

object main extends App {
  import MyList._

//  println(take(MyList(1,2,3,4), 4))
//
//  println(flattenList(MyList(MyList(1,2,3), MyList(4,5,6))))

  println(take(MyList(1,2,3,4), 3))

}


