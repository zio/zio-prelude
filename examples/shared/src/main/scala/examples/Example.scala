//package examples
//
//import refined.Assertion.{greaterThan, lessThan, matches}
//import refined._
//
//object Example {
//  val Amount = NewtypeSmart[Int](greaterThan(11) && lessThan(20))
//  type Amount = Amount.Type
//
//  def main(args: Array[String]): Unit = {
//    val amount = Amount(12)
//
//    def useAmount(amount: Amount): Int =
//      amount.value + 10
//
//    println(useAmount(amount))
//  }
//}
//
//object EmailExample {
//  def main(args: Array[String]): Unit = {
//    val Email = SubtypeSmart[String](matches("^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$"))
//    type Email = Email.Type
//
//    val email: Email = Email("kit#gmail.com")
//    println(email.toUpperCase)
//  }
//}
