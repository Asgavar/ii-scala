package money {
  trait Currency {
    val name: String
    val decimalPlaces: Int = 2
  }

  object USD extends Currency { val name = "United States dollar" }
  object EUR extends Currency { val name = "Euro" }
  object PLN extends Currency { val name = "Polish złoty" }

  case class Money(amount: BigDecimal, currency: Currency)(implicit currencyConverter: CurrencyConverter) {
    override def toString = s"${amount} ${currency.name}s"

    def +(that: Money) =
      Money(
        (amount + that.amount * currencyConverter.convert(that.currency, currency))
          .setScale(currency.decimalPlaces, BigDecimal.RoundingMode.HALF_UP), currency)

    def -(that: Money) =
      Money(
        (amount - that.amount * currencyConverter.convert(that.currency, currency))
          .setScale(currency.decimalPlaces, BigDecimal.RoundingMode.HALF_UP), currency)

    def *(multiplier: Int) =
      Money(
        (amount * multiplier)
          .setScale(currency.decimalPlaces, BigDecimal.RoundingMode.HALF_UP), currency)

    def as(targetCurrency: Currency) =
      Money(
        (amount * currencyConverter.convert(currency, targetCurrency))
          .setScale(targetCurrency.decimalPlaces, BigDecimal.RoundingMode.HALF_UP), targetCurrency)

    private def compare(that: Money) =
      amount.compare(that.amount * currencyConverter.convert(that.currency, currency))

    def <(that: Money) =
      compare(that) == -1

    def >(that: Money) =
      compare(that) == 1
  }

  case class CurrencyConverter(conversion: Map[(Currency, Currency), BigDecimal]) {
    def convert(from: Currency, to: Currency): BigDecimal =
      if (from == to) BigDecimal(1) else conversion.get((from, to)).get
  }

  package object dsl {
    implicit class MoneyableDouble(d: Double)(implicit currencyConverter: CurrencyConverter) {
      def apply(currency: Currency): Money = Money(BigDecimal(d), currency)
    }

    implicit class MoneyableInt(d: Int)(implicit currencyConverter: CurrencyConverter) {
      def apply(currency: Currency): Money = Money(BigDecimal(d), currency)
    }
  }

  package config {
    package object defaults {
      implicit val cc = new CurrencyConverter(
        Map(
          (USD, EUR) -> 0.825033,
          (USD, PLN) -> 3.68650,
          (EUR, USD) -> 1.21207,
          (EUR, PLN) -> 4.46831,
          (PLN, USD) -> 0.271260,
          (PLN, EUR) -> 0.223799
        )
      )

      val $ = USD
      val ę = EUR  // proper unicode sign kept failing with "error: illegal character '\u20ac'"
      val zł = PLN
    }
  }
}

object Lab07 {
  import money._
  import money.dsl._
  import money.config.defaults._

  def main(args: Array[String]) = {
    val compoundExpr1 = 42(PLN) as USD as PLN
    val compoundExpr2 = (((100(PLN) as EUR) + 25(USD)) * 10) - 2(ę)
    println(compoundExpr1)
    println(compoundExpr2)

    val d = 42.0
    println(d)
    println(d($))

    val usd1 = 100.01(USD)
    val eur1 = 200(EUR)
    val pln1 = 3(PLN)

    println(usd1)
    println(eur1)
    println(pln1)

    val sum1: Money = 100.01(USD) + 200(EUR)
    val sum2: Money = 100.01(zł) + 200($)
    val sum3: Money = 5(zł) + 3(PLN) + 20.5(USD)

    println(sum1)
    println(sum2)
    println(sum3)

    val sub1: Money = 300.01(USD) - 200(EUR)
    val sub2: Money = 100.01(zł) - 200($)
    val sub3: Money = 200(ę) - 100.01(zł)

    println(sub1)
    println(sub2)
    println(sub3)

    val mult1: Money = 30(zł) * 20
    val mult2: Money = 20($) * 11
    val mult3: Money = 5(ę) * 10

    println(mult1)
    println(mult2)
    println(mult3)

    val conv1: Money = 150.01(USD) as PLN
    val conv2: Money = 120.01(USD) as ę

    println(conv1)
    println(conv2)

    val compare1: Boolean = 300.30(USD) > 200(ę)
    val compare2: Boolean = 300.30($) < 200(EUR)
    val compare3: Boolean = 100(PLN) < 25.0(ę)

    println(compare1)
    println(compare2)
    println(compare3)
  }
}
