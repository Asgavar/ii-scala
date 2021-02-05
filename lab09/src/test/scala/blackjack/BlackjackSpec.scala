import org.scalatest._
import org.scalatest.matchers.should.Matchers._

import blackjack._
import cards._

class BlackjackSpec extends flatspec.AnyFlatSpec {
  val card1 = Card(Hearts, Queen)
  val card2 = Card(Diamonds, Spot(4))
  val card3 = Card(Spades, Ace)

  "A Blackjack game instance" should "calculate points for spot and face cards" in {
    val game = Blackjack(1)

    game.cardValue(card1, 0) should equal(10)
    game.cardValue(card2, 0) should equal(4)
  }

  it should "always pick a points number for Ace that's currently better for the player" in {
    val game = Blackjack(1)

    game.cardValue(card3, 0) should equal(11)
    game.cardValue(card3, 20) should equal(1)
  }

  it should "be able to calculate the value of the whole game" in {
    val game = Blackjack(1)

    game.gameValue(List(card1, card2, card3)) should equal(15)
  }

  it should "reveal the first winning sequence in a given cards combination" in {
    val game = Blackjack(5)

    game.gameValue(game.first21()) should equal(21)
  }
}
