import exceptions._

class Account(val bank: Bank, initialBalance: Double) {
  class Balance(var amount: Double) {}

  val balance = new Balance(initialBalance)

  // for project task 1.2: implement functions
  // for project task 1.3: change return type and update function bodies
  def withdraw(amount: Double): Either[Unit, String] = this.synchronized{
    if (amount < 0 )
      return Right("negative")
    else if (amount > balance.amount)
      return Right("greater")
    Left(balance.amount -= amount)
  }

  def deposit (amount: Double): Either[Unit, String] = this.synchronized{
    if (amount < 0)
      return Right("negative")
    Left(balance.amount += amount)
  }
  
  def getBalanceAmount: Double = this.synchronized{balance.amount}

  def transferTo(account: Account, amount: Double) = {
    bank.addTransactionToQueue(this, account, amount)
  }
}
