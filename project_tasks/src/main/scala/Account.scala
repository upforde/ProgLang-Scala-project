import exceptions._

class Account(val bank: Bank, initialBalance: Double) {
  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)

  // for project task 1.2: implement functions
  // for project task 1.3: change return type and update function bodies
  def withdraw(amount: Double): Either[Unit, String] = this.synchronized{
    // If the amount is negative
    if (amount < 0 )
      // return String indicating the balance is negative
      return Right("negative")
    // If the amount is greater than the balance amount
    else if (amount > balance.amount)
      // return String indicating the amount is greater than the balance
      return Right("greater")
    // Otherwise execute the withdrawl
    Left(balance.amount -= amount)
  }

  def deposit (amount: Double): Either[Unit, String] = this.synchronized{
    // If the amount is negative
    if (amount < 0)
      // return String indicating the balance is negative
      return Right("negative")
    //  Otherwise execute the deposit
    Left(balance.amount += amount)
  }
  
  def getBalanceAmount: Double = this.synchronized{balance.amount}

  def transferTo(account: Account, amount: Double) = {
    bank.addTransactionToQueue(this, account, amount)
  }
}
