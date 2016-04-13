

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]) : Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  
  def apply[A] (as: A*) : List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
    
    
    
    
  def tail[A](as : List[A]) : List[A] = as match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }
  
    def setHead[A] ( as : List[A], a : A) : List[A] = as match {
      case Nil => Cons(a, Nil)
      case Cons(x, xs) => Cons(a, xs)
    }
    
  def drop[A] (l: List[A], n: Int) : List[A] =
    if (n == 0) l
    else drop(tail(l), n-1)
    
  def dropWhile[A](l: List[A], f: A => Boolean) : List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else Cons(x, dropWhile(xs, f))
  }
  
  def init[A] (l : List[A]) : List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }
  
  
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
  
  def length[A](as: List[A]): Int = 
    foldRight(as, 0)((x,y) => y + 1) 
    
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B) : B = as match{
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }
   def sumFL(ints: List[Int]) : Int = foldLeft(ints, 0)(_ + _)
   def productFL(ds: List[Double]) : Double = foldLeft(ds, 1.0)(_ * _)
   
   def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((x,y) => Cons(y,x))
  }
  
  def append[A](a1: List[A], a2: List[A]) : List[A] = {
    foldRight(a1, a2)( Cons(_,_))
  }
  
  def concat[A](l: List[List[A]]) : List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => 
  }
}





