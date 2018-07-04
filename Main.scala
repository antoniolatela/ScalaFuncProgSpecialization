package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    def loop(row: Int, array: Array[Int]): Int = {
      if (row == r) array(c)
      else if (array.length == 1) loop(row + 1, Array(1, 1))
      else {
        val a = new Array[Int](array.length + 1)
        for ((v, i) <- a.zipWithIndex) {
          if (i == 0 || i == a.length - 1) a(i) = 1
          else a(i) = array(i - 1) + array(i)
        }
        if (row == r) a(c) else loop(row + 1, a)
      }
    }
    loop(0, Array(1))
  }
  
  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop (c:Int, ch:List[Char]): Boolean = {
      if (ch.length == 0 && c == 0) true
      else if (ch.length == 0 && c != 0) false
      else if (c == -1) false
      else if (ch(0).equals('(')) loop(c+1, ch.tail)
      else if (ch(0).equals(')')) loop(c-1, ch.tail)
      else loop(c, ch.tail)
    }
    loop(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int =  {
      if (money == 0) 1
      else if (coins.length == 0 || money < 0) 0
      else countChange(money, coins.tail) + countChange(money - coins(0), coins)
  }
}
