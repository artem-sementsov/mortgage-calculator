import java.lang.Math._

import scala.util.control.Breaks._

object Main {

  val MortgagePercentInYear = 0.084
  val InflationPercentInYear = 0.05

  lazy val mortgagePercentInMonth = MortgagePercentInYear / 12 //pow(1 + MortgagePercentInYear, 1D / 12) - 1
  lazy val inflationPercentInMonth = pow(1 + InflationPercentInYear, 1D / 12) - 1

  def main(args: Array[String]): Unit = {
    val payment = 30000
    val duration = 240
    val oneTimeCommission = 200000

    val loan = maxAmountByMonthlyPayment(payment, duration)
    println(loan)
    val profit = investmentAttractiveness(loan, duration, payment, oneTimeCommission)

    println()
    println("results:")
    println("you get loan in %.2f coins".format(loan))
    println("for " + duration + " months")
    println("with mortgage " + MortgagePercentInYear * 100 + "% per year")
    println("and expected average inflation " + InflationPercentInYear * 100 + "% per year")
    println("and also you paid commission " + oneTimeCommission + " coins")
    println("your profit = " + profit)
  }

  def maxAmountByMonthlyPayment(payment: Double, durationInMonths: Int): Double = {
    val epsilon = 0.0001
    val accInflation = accPercent(durationInMonths, inflationPercentInMonth)

    def calcPayment(loan: Double): Double = {
      var curPayment = payment
      var curTax = 0D
      var rest = loan

      for (_ <- 1 to durationInMonths) {
        curPayment = curPayment * (1 + inflationPercentInMonth)
        curTax = rest * mortgagePercentInMonth
        rest = rest - curPayment + curTax
      }
      println("rest = " + rest)

      // 3 is good for convergence
      loan - rest / 3
    }

    var maxLoan = payment * accInflation
    println("start max loan = " + maxLoan)

    breakable {
      for (i <- 0 until 1000) {
        val loan = calcPayment(maxLoan)
        println(i + ") new max loan = " + loan)

        if (abs((loan - maxLoan) / loan) < epsilon) {
          maxLoan = loan
          break()
        }
        maxLoan = loan
      }
    }

    maxLoan
  }

  /** Calculates attractiveness of investment into realty
   * if result < 1 then investment is profitable
   */
  def investmentAttractiveness(loan: Double, durationInMonths: Int, payment: Double, oneTimeCommission: Double): Double = {
    val pay = (payment * accPercent(durationInMonths, inflationPercentInMonth)) + oneTimeCommission
    val loanWithInflation = loan * distancePercent(durationInMonths, inflationPercentInMonth)

    println("pay = " + pay)
    println("loanWithInflation = " + loanWithInflation)

    pay / loanWithInflation
  }

  def accPercent(durationInMonths: Int, percent: Double): Double = {
    (1 to durationInMonths).map(pow(1 + percent, _)).sum
  }

  def distancePercent(durationInMonths: Int, percentInMonth: Double): Double = {
    pow(1 + percentInMonth, durationInMonths)
  }
}