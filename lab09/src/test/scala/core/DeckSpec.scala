import org.scalatest._
import org.scalatest.matchers.should.Matchers._

import cards._
import deck._

class DeckSpec extends flatspec.AnyFlatSpec {
  val shuffledDeck = Deck()

  "A shuffled standard deck" should "have 4 Queens, 4 Jacks, 4 Kings and 4 Aces" in {
    shuffledDeck.amountOfFace(Queen) should equal(4)
    shuffledDeck.amountOfFace(Jack) should equal(4)
    shuffledDeck.amountOfFace(King) should equal(4)
    shuffledDeck.amountOfFace(Ace) should equal(4)
  }

  it should "have 4 copies of each number card" in {
    for (n <- 2 to 10) {
      shuffledDeck.amountOfNumerical(n) should equal(4)
    }
  }

  it should "have 52 cards with 16 of them being faces" in {
    shuffledDeck.amountWithNumerical should equal(52 - 16)
    shuffledDeck.amountWithFace should equal(16)
  }

  it should "recognize itself as a standard deck" in {
    shuffledDeck.isStandard should be(true)
  }

  "An initially empty deck" should "discover duplicated cards" in {
    val card = Card(Spades, Spot(3))
    val emptyDeck = new Deck(Nil)
      .push(card)
      .push(Spades, Spot(3))

    emptyDeck.duplicatesOfCard(card) should equal(1)
  }

  it should "properly react to push/pull operations" in {
    val card1 = Card(Hearts, Queen)
    val card2 = Card(Diamonds, King)
    val deck = new Deck(Nil)

    deck.push(card1).firstCard should be(Some(card1))
    deck.push(card1).push(card2).firstCard should be(Some(card2))
    deck.push(card1).push(card2).pull().firstCard should be(Some(card1))
  }

  it should "properly behave even when there's no cards left" in {
    val deck = new Deck(Nil)

    deck.firstCard should be(None)
    deck.pull().firstCard should be(None)
    deck.pull().allCards should be(deck.allCards)
  }
}
