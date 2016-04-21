

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(a) => 1
    case Branch(l,r) => size(l) + size(r) + 1
  }
  
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(i) => i
    case Branch(l,r) => maximum(l) max maximum(r)
  }
  
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(i) => 1
    case Branch(l,r) => (1 + depth(l)) max (1 + depth(r))
  }
  
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
  
  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(a) => l(a)
    case Branch(b1,b2) => b(fold(b1)(l)(b),fold(b2)(l)(b)) 
  }
  
  def sizeF[A](t: Tree[A]): Int = fold(t)(_ => 1)(_ + _ + 1)
  def maximumF(t: Tree[Int]): Int = fold(t)(x => x)( _ max _ )
  def depthF[A](t: Tree[A]): Int = fold(t)(_ => 1)((x,y) => (x + 1) max (y + 1))
  def mapF[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(x => Leaf(f(x)): Tree[B])(Branch(_,_))
}
  
