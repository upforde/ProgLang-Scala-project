import java.util.concurrent.atomic.AtomicReference
object Task2 {

    var counter: Int = 0
    
    def main(args: Array[String]) 
    {
        val thread = Threading(GenerateArray(10))
        println(thread.start())

    }

    def Threading(body: =>Unit): Thread = 
    {
        val t = new Thread {override def run() = body}
        t
    }

    def GenerateArray (number : Int) : Array[Int] =
    {
        val arr = Array.ofDim[Int](number)
        for (i <- 1 to number) arr(i-1) = i
        arr
    }

    def increaseCounter(): Unit = {
        counter += 1
    }

    def increaseCounterSafe(): Unit = {
        val atomicCounter =  new AtomicReference(counter)
        val priorCounter = counter
        val newCounter = priorCounter + 1;
        if(atomicCounter.compareAndSet(priorCounter, newCounter)) {
            return;
        }
    }

    def threadTesting () : Unit =
    {
        val thread1 = new Thread {increaseCounter()}
        val thread2 = new Thread {increaseCounter()}
        val thread3 = new Thread {println(counter)}

        thread1.start
        thread2.start
        thread3.start


    }

    def printCounter () : Int =
    {
        counter
    }

    def Deadlock () = {
        
    }

}