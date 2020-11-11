object Main  
{ 
    def main(args: Array[String]) 
    { 
        println(GenerateArray(50)); 
    }

    def GenerateArray (number:Int) : Array =
    {
        val x = for (x <- 1 to number)
        return x
    }
} 