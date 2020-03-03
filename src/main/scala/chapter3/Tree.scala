package chapter3

sealed trait Tree[+A] {
  def size: Int
  def maximum: Int
  def depth: Int
  def map[B](f: A => B): Tree[B]
}

case class Leaf[A](value: A) extends Tree[A] {
  override def size: Int = 1

  override def maximum: Int = this match {
    case Leaf(a: Int) => a
    case _ => throw new NoSuchMethodError
  }

  override def depth: Int = 1

  override def map[B](f: A => B): Tree[B] = this match {
    case Leaf(a) => Leaf(f(a))
  }
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  override def size: Int = this match {
    case Branch(l, r) => 1 + l.size + r.size
  }

  override def maximum: Int =  this match {
    case Branch(l, r) => l.maximum max r.maximum
  }

  override def depth: Int = {
    def loop(acc: Int, branch: Tree[A]): Tree[Int] = branch match {
      case Leaf(_) => Leaf(acc + 1)
      case Branch(l, r) => Branch(loop(acc + 1, l), loop(acc + 1, r))
    }
    loop(0, this).maximum
  }

  override def map[B](f: A => B): Tree[B] = this match {
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  def foldLeft[B] (f: A => B, g: (B,B) => B): B = foldLeft(this)(f)(g)

  private def foldLeft[B](tree: Tree[A])(f: A => B)(combine: (B,B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(l, r) => combine(foldLeft(l)(f)( combine), foldLeft(l)(f)(combine))
  }

}

object main2 extends App {
  val tree =
    Branch(
      Branch(
        Branch(
          Leaf(3),
          Leaf(9)
        ),
        Leaf(5)
      ),
      Branch(
        Leaf(10),
        Leaf(1)
      )
    )

  val leaf = Leaf("hi")

  println(tree.foldLeft(_+1, (a: Int,b: Int) => (a * b)))

}


