

import scala.annotation.tailrec

object Sort {
  
  def isSorted[A] (as: Array[A], ordered: (A,A) => Boolean) : Boolean = {
    
    @annotation.tailrec
    def loop(n : Int) : Boolean = {
      if (n == as.length -1) true
      else if ( ! ordered(as(n), as(n + 1))) false
      else loop(n+1)
    }
    
    loop(0)
    
  }
  
  def main (args: Array[String]) : Unit ={
    println (isSorted(Array(1,2,3), (n1: Int, n2: Int) => n1 <= n2)) 
    println (isSorted(Array(1,2,3,2), (n1: Int, n2: Int) => n1 <= n2)) 
    println (isSorted(Array("a","b","c"), (s1: String, s2: String) => s1 <= s2)) 
    println (isSorted(Array("a","b","d","c"), (s1: String, s2: String) => s1 <= s2)) 
    println (isSorted(Array("a","b","b","c"), (s1: String, s2: String) => s1 <= s2)) 
  }
}