object Task1 {
  def main(args: Array[String]) 
    { 
        println(GenerateArray(50)mkString(" "));
        println(SumArray(GenerateArray(50)));
        println(RecursionSumArray(GenerateArray(50)));
        println(Fib(9)); 
    }

    def GenerateArray (number : Int) : Array[Int] =
    {
        val arr = Array.ofDim[Int](number)
        for (i <- 1 to number) arr(i-1) = i
        arr
    }

    def SumArray (arr : Array[Int]) : Int =
    {
        var sum = 0
        for (i <- 1 to arr.length) sum = sum + arr(i-1)
        sum
    }

    def RecursionSumArray (arr : Array[Int]) : Int =
    {
        if (arr.length == 1) arr.head else
        arr.head + RecursionSumArray(arr.tail)
    }


    def Fib (n : BigInt) : BigInt =
    {
        if (n < 3) 1 else
        Fib(n - 1) + Fib(n - 2)
    }


}