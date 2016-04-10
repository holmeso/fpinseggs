

object TestList {
  
  def main(args: Array[String]) : Unit = {
  
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42     
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println (x)
    
    println ("tailing")
    println (List.tail(List(1,2,3,4,5)))
    println (List.tail(Nil))
    println (List.tail(List("a","b","c")))
    
    println ("set heading")
    println (List.setHead(List(1,2,3,4,5), 100))
    
    println ("drop")
    println (List.drop(List(1,2,3,4,5), 1))
    println (List.drop(List(1,2,3,4,5), 2))
    println (List.drop(List(1,2,3,4,5), 3))
    println (List.drop(List(1,2,3,4,5), 4))
    println (List.drop(List(1,2,3,4,5), 5))
    println (List.drop(List(1,2,3,4,5), 6))
    
    
    
    println ("drop while")
    println (List.dropWhile(List(1,2,3,4,5), (a:Int) => a < 2))
    println (List.dropWhile(List(1,2,3,4,5), (a:Int) => a > 3))
    
    println ("init")
    println (List.init(List(1,2,3,4,5)))
    
    
    println ("foldRight")
    println (List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))
    
    println ("length foldRight")
    println (List.length(List(1,2,3)))
    println (List.length(List(5,5,2,4,1)))
    
    println ("foldLeft")
    println (List.foldLeft(List(1,2,3), 0)((x,y) => x + y))
  }

}