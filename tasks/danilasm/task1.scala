object task1 {
  def main(args: Array[String]):Unit = {
    // a) Generating the array. (1 to 50) is the
    // same as doing a for loop from i = 1 until i == 50
    array = createArray(50)

    println(sum(array))
    println(recSum(array))
    println(fib(10000))
  }

  // Function that creates an array of size n filled with ints 
  // increasing from 1 to n
  def createArray(n: Int):Array[Int] = {
    val array = new Array[Int](n)
    for (a <- 1 to n) array(a-1) = a
    array
  }

  // b) fucntion that sums an array using a for loop
  def sum(a:Array[Int]) = {
    var sum = 0
    for (i<-a) sum += i
    sum
  }

  // c) Function that sums an array recursively
  def recSum(a:Array[Int]):Int = a match {    //Similarly to oz, the scala programing
    case Array() => 0                         //language implements pattern matching
    case ht => ht.head + recSum(ht.tail)      //making this quite easy to implement
  }

  // d) Function that returns the nth number of the Fibonacci sequence.
  def fib (n : BigInt) : BigInt =
  {
      if (n < 3) 1 else
      Fib(n - 1) + Fib(n - 2)
  }

  // d) The difference between Int and BigInt 
  // is the amount of memory allocated to the
  // number, or in other words, how big of a
  // number one can have before experiencing
  // an intiger overflow. Int has 32 bits of 
  // memory allocated, while BigInt has 64 bits.
}