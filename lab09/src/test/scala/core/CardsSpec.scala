import org.scalatest._
import org.scalatest.matchers.should.Matchers._

import cards._

class CardsSpec extends flatspec.AnyFlatSpec {
  "A spot card" should "not be able to have its value set to more than 10" in {
    a[IllegalArgumentException] should be thrownBy {
      Card(Hearts, Spot(42))
    }
  }

  "A card" should "be able to render itself to a nice string form" in {
    val card1 = Card(Hearts, Queen)
    val card2 = Card(Clubs, Spot(3))

    card1.toHumanReadable should be("♥Q")
    card2.toHumanReadable should be("♣3")
  }
}
