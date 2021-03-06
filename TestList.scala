

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
    println ("reverse")
    println (List.reverse(List(1,2,3)))
    println ("append")
    println (List.concat(List(List(1,2,3), List(4,5,6))))
    println ("add1")
    println (List.add1(List(1,2,3)))
    println ("toStringList")
    println (List.toStringList(List(1.0,2.1,3.2)))
    
    println ("filter")
    println (List.filter(List(1,2,3)) ((a:Int) => a % 2 != 0))
    println ("zip width")
    println (List.zipWidth(List(1,2,3), List(2,3,4)) (_ + _))
    println (List.zipWidth(List(1,2,3), List(2,3,4)) (_ - _))
    println (List.zipWidth(List(1,2,3), List(2,3,4)) (_ / _))
    println (List.zipWidth(List(1,2,3), List(2,3,4)) (_ * _))
    
     println("size")
     println(Tree.size(Leaf(1)))
     println("size - done")
  }

}