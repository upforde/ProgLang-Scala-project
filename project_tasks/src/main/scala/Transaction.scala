import exceptions._
import scala.collection.mutable._

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {
  // project task 1.1
  // Add datastructure to contain the transactions
  var queue = new Queue[Transaction]()

  // Remove and return the first element from the queue
  def pop: Transaction = this.synchronized{queue.dequeue}

  // Return whether the queue is empty
  def isEmpty: Boolean = this.synchronized{queue.isEmpty}

  // Add new element to the back of the queue
  def push(t: Transaction): Unit = this.synchronized{queue.enqueue(t)}

  // Return the first element from the queue without removing it
  def peek: Transaction = this.synchronized{queue.front}

  // Return an iterator to allow you to iterate over the queue
  def iterator: Iterator[Transaction] = this.synchronized{queue.iterator}
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING
  var attempt = 0

  override def run: Unit = {
    def doTransaction() = this.synchronized{
      // Keeping track of the success of both withdrawl
      // and deposit
      var transactionSuccess = false
      // Loop the allowed amount of times
      while(this.attempt != this.allowedAttemps){
        // Increment the attempt
        this.attempt += 1
        // If the withdrawl has yet to succeed
        if (!transactionSuccess){
          // Try withdrawl
          from.withdraw(amount) match{
            // If success, attempt to deposit money.
            // Since deposit is restricted in the same way as withdrawl
            // (fails on negative amount), if withdrawl fails, so will deposit.
            // Therefore, if withdrawl succeeds
            case Left(()) => to.deposit(amount) match{
                // In case of success, increase the attempts 
                // to the allowed amount and set the boolean
                // to true
                case Left(()) => {
                  transactionSuccess = true
                  this.attempt = this.allowedAttemps
                }
                // In case of failure, make sure the next if statement fails
                case Right(string) => transactionSuccess = false
              }
            // If failure
            case Right(string) => string match {
              // If the amount was negative, increase the attempts
              // to the allowed amount
              case "negative" => this.attempt = this.allowedAttemps
              // If the amount to withdraw was greater than the account
              // balance, wait a bit, in case another parrallell transaction
              // increases the balance of the acount enough
              case "greater" => Thread.sleep(100)
            }
          }
        }
      }
      // If the withdrawl has succeeded (meaning that the deposit succeeded as well)
      if (transactionSuccess)
        // Set the status of the transaction to SUCCESS
        this.status = TransactionStatus.SUCCESS
      // Otherwise, the transaction failed
      else this.status = TransactionStatus.FAILED
    }

    if (this.status == TransactionStatus.PENDING) {
      // Since doTransaction is synchronised, it's
      // thread friendly
      doTransaction
      Thread.sleep(50) // you might want this to make more room for
                        // new transactions to be added to the queue
    }
  }
}
