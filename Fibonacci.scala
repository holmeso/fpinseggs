



object Fibonacci {
  def fib(n: Int): Int = {
      
    @annotation.tailrec
    def go (f1: Int, f2: Int, c: Int): Int = {
//      println ("in go with f1 " + f1 + " f2 " + f2 + " c " + c)
      
    
      if (c == n)
        f1
      else {
        go(f2, f1 + f2, c+1)
      }
    }
    
    go(0,1,1)
  }
  
  def main(args: Array[String]) : Unit = {
    println (fib(4))
  }
}