package pizzeria {

  package promos {
    abstract case class Promo(promoDescription: String) {
      def drinkPrice(drink: food.drinks.Drink): Double = drink.price
      def pizzaPrice(pizza: food.pizza.Pizza): Double = pizza.price
      override def toString = promoDescription
    }

    trait SimpleDrinkDiscountPromo {
      this: Promo =>

      val percentOffForDrinks: Int
      override def drinkPrice(drink: food.drinks.Drink) = (1 - percentOffForDrinks/100.0) * drink.price
    }

    trait SimplePizzaDiscountPromo {
      this: Promo =>

      val percentOffForPizzas: Int
      override def pizzaPrice(pizza: food.pizza.Pizza) = (1 - percentOffForPizzas/100.0) * pizza.price
    }

    object SamePricePromo extends Promo("no promo at all")

    object StudentDiscountPromo extends Promo("discount for students") with SimplePizzaDiscountPromo {
      val percentOffForPizzas = 5
    }

    object SeniorDiscountPromo extends Promo("discount for seniors") with SimpleDrinkDiscountPromo with SimplePizzaDiscountPromo {
      val percentOffForDrinks = 7
      val percentOffForPizzas = 7
    }
  }

  package orders {
    object Constants {
      val phoneNumberPattern = raw"(\+?48 ?)?(\d{3}[- ]){2}\d{3}".r
    }

    class Order(
      name: String,
      address: String,
      phone: String,
      pizzas: Option[List[food.pizza.Pizza]],
      drinks: Option[List[food.drinks.Drink]],
      discount: Option[promos.Promo],
      specialInfo: String,
    ) {
      require(Constants.phoneNumberPattern.matches(phone))

      override def toString =
        s"${name}\n${phone}\nAddress: ${address}\nPizzas:\n" + (
          pizzas match {
                 case None => ""
                 case Some(ps) => ps.foldLeft("")((str, p) => str + s"  - ${p}\n")
          }) + "Drinks:\n" + (
          drinks match {
            case None => ""
            case Some(ds) => ds.foldLeft("")((str, d) => str + s"  - ${d}\n")
          }) + (
          discount match {
            case None => "With no discount"
            case Some(dis) => s"Discount applied: ${dis}"
          }) + s"\nOrder value: ${price}$$\nAdditional notes: ${specialInfo}"

      def extraMeatPrice: Option[Double] = pizzas match {
        case None => None
        case Some(pizzas) => pizzas.map(
          p => p.extraMeat match {
            case None => 0
            case Some(extraMeat) => extraMeat.price
          }
        ).sum match {
          case 0 => None
          case sum => Some(sum)
        }
      }

      def pizzasPrice: Option[Double] = pizzas match {
        case None => None
        case Some(pizzas) => Some(pizzas.map(_.price).sum)
      }

      def drinksPrice: Option[Double] = drinks match {
        case None => None
        case Some(drinks) => Some(drinks.map(_.price).sum)
      }

      def priceByKind(kind: food.pizza.kinds.PizzaKind): Option[Double] = pizzas match {
        case None => None
        case Some(pizzas) => pizzas.filter(_.kind == kind) match {
          case Nil => None
          case pizzasOfThisKind => Some(pizzasOfThisKind.map(_.price).sum)
        }
      }

      val price: Double = {
        val discountInUse = discount match {
          case None => promos.SamePricePromo
          case Some(promo) => promo
        }

        (pizzas match {
          case None => 0
          case Some(pizzas) => pizzas.map(p => discountInUse.pizzaPrice(p)).sum
        }) + (drinks match {
          case None => 0
          case Some(drinks) => drinks.map(d => discountInUse.drinkPrice(d)).sum
        })
      }
    }
  }

  package food {

    package pizza {
      case class Pizza(
        kind: kinds.PizzaKind,  // "type" turned out to be a reserved keyword, so I renamed it to "kind"
        size: sizes.PizzaSize,
        crust: crusts.PizzaCrust,
        extraMeat: Option[extras.meats.Meat],
        extraTopping: Option[extras.toppings.Topping],
      ) {
        override def toString() =
          s"${size.sizeCode}-sized, ${crust.crustName}-crust Pizza ${kind.name} with " + (extraMeat match {
            case None => "no additional meat"
            case Some(meat) => s"${meat.name}"
          }) + " and " + (extraTopping match {
            case None => "no extra toppings"
            case Some(topping) => s"${topping.name}"
          })

        val price = kind.price(size, crust) + (extraMeat match {
          case None => 0
          case Some(meat) => meat.price
        }) + (extraTopping match {
          case None => 0
          case Some(topping) => topping.price
        })
      }

      package extras {

        package meats {
          abstract case class Meat(name: String, price: Double)

          object Salami extends Meat("Salami", 1)
        }

        package toppings {
          abstract case class Topping(name: String, price: Double)

          object Ketchup extends Topping("Ketchup", 0.5)
          object Garlic extends Topping("Garlic", 0.5)
        }

      }

      package sizes {
        sealed abstract case class PizzaSize(sizeCode: String)

        object SmallPizzaSize extends PizzaSize("S")
        object RegularPizzaSize extends PizzaSize("M")
        object LargePizzaSize extends PizzaSize("XL")
      }

      package crusts {
        sealed abstract case class PizzaCrust(crustName: String)

        object ThinPizzaCrust extends PizzaCrust("thin")
        object ThickPizzaCrust extends PizzaCrust("thick")
      }

      package kinds {

        abstract class PizzaKind {
          val name: String
          def price(size: sizes.PizzaSize, crust: crusts.PizzaCrust): Double
        }

        trait SimplyPricedPizzaKind {
          this: PizzaKind =>

          val basePrice: Double
          override def price(size: sizes.PizzaSize, crust: crusts.PizzaCrust) = size match {
            case sizes.SmallPizzaSize => 0.9 * basePrice
            case sizes.RegularPizzaSize => basePrice
            case sizes.LargePizzaSize => 1.5 * basePrice
          }
        }

        object MargaritaPizzaKind extends PizzaKind with SimplyPricedPizzaKind {
          val name = "Margarita"
          val basePrice = 5
        }

        object PepperoniPizzaKind extends PizzaKind with SimplyPricedPizzaKind {
          val name = "Pepperoni"
          val basePrice = 6.5
        }

        object FunghiPizzaKind extends PizzaKind with SimplyPricedPizzaKind {
          val name = "Funghi"
          val basePrice = 7
        }

      }

    }

    package drinks {
      abstract case class Drink(name: String, price: Double) {
        override def toString = name
      }

      object Lemonade extends Drink("Lemonade", 2)
    }

  }

}

object Lab06 {
  import scala.util.Try

  import pizzeria._

  def main(args: Array[String]) = {
    val pizza = new food.pizza.Pizza(
      food.pizza.kinds.FunghiPizzaKind,
      food.pizza.sizes.LargePizzaSize,
      food.pizza.crusts.ThinPizzaCrust,
      None,
      Some(food.pizza.extras.toppings.Garlic),
    )

    println(pizza)
    println(pizza.price)

    val badPhoneNumber = Try {
      new orders.Order("", "", "00 0-01", Some(List(pizza)), None, None, "")
    }

    println(badPhoneNumber.isFailure)

    val order1 = new orders.Order(
      "Hymel Jadwiga",
      "Łączna 43, Lipinki Łużyckie",
      "111-222-333",
      Some(List(new food.pizza.Pizza(
                  food.pizza.kinds.MargaritaPizzaKind,
                  food.pizza.sizes.SmallPizzaSize,
                  food.pizza.crusts.ThickPizzaCrust,
                  Some(food.pizza.extras.meats.Salami),
                  Some(food.pizza.extras.toppings.Ketchup)),
                pizza,
      )),
      Some(List(food.drinks.Lemonade)),
      Some(promos.SeniorDiscountPromo),
      "zaraz koło poczty objazd",
    )
    println(order1)
    println(order1.extraMeatPrice)
    println(order1.drinksPrice)
    println(order1.pizzasPrice)
    println(order1.priceByKind(food.pizza.kinds.FunghiPizzaKind))
    println(order1.priceByKind(food.pizza.kinds.MargaritaPizzaKind))
    println(order1.priceByKind(food.pizza.kinds.PepperoniPizzaKind))

    val order2 = new orders.Order(
      "Thirsty Student",
      "42, Lemonade fans avenue",
      "+48 999-998-997",
      Some(List(pizza)),
      Some(List(food.drinks.Lemonade,
                food.drinks.Lemonade,
                food.drinks.Lemonade,
                food.drinks.Lemonade,
                food.drinks.Lemonade)),
      Some(promos.StudentDiscountPromo),
      "",
    )

    println(order2)
    println(order2.extraMeatPrice)
    println(order2.drinksPrice)
    println(order2.pizzasPrice)
    println(order2.priceByKind(food.pizza.kinds.FunghiPizzaKind))
    println(order2.priceByKind(food.pizza.kinds.MargaritaPizzaKind))
    println(order2.priceByKind(food.pizza.kinds.PepperoniPizzaKind))

    val order3 = new orders.Order(
      "Undecided Human Being",
      "",
      "+48 123 456 789",
      None,
      None,
      None,
      ""
    )

    println(order3)
    println(order3.extraMeatPrice)
    println(order3.drinksPrice)
    println(order3.pizzasPrice)
    println(order3.priceByKind(food.pizza.kinds.FunghiPizzaKind))
    println(order3.priceByKind(food.pizza.kinds.MargaritaPizzaKind))
    println(order3.priceByKind(food.pizza.kinds.PepperoniPizzaKind))

    val order4 = new orders.Order(
      "John Smith",
      "",
      "997 998 999",
      Some(List(pizza)),
      None,
      None,
      ""
    )

    println(order4)
    println(order4.extraMeatPrice)
    println(order4.drinksPrice)
    println(order4.pizzasPrice)
    println(order4.priceByKind(food.pizza.kinds.FunghiPizzaKind))
    println(order4.priceByKind(food.pizza.kinds.MargaritaPizzaKind))
    println(order4.priceByKind(food.pizza.kinds.PepperoniPizzaKind))
  }
}
