package cards {
  abstract class Color
  abstract class Rank

  case class Card(color: Color, rank: Rank)

  case object Clubs extends Color
  case object Diamonds extends Color
  case object Hearts extends Color
  case object Spades extends Color

  case object Jack extends Rank
  case object Queen extends Rank
  case object King extends Rank
  case object Ace extends Rank
  case class Spot(n: Int) extends Rank {
    require((2 to 10).contains(n), "I don't think so")
  }
}

package deck {
  import cards._

  class Deck(cards: List[Card]) {
    def pull() =
      new Deck(cards.tail)

    def push(c: Card) =
      new Deck(c :: cards)

    def push(color: Color, rank: Rank) =
      new Deck(Card(color, rank) :: cards)

    val isStandard: Boolean =
      cards.length == 52 && cards.distinct.length == 52

    def duplicatesOfCard(card: Card) =
      scala.math.max(0, cards.count(_ == card) - 1)

    def amountOfColor(color: Color) =
      cards.count(_.color == color)

    def amountOfNumerical(numerical: Int) =
      cards.count(_.rank == Spot(numerical))

    val amountWithNumerical: Int =
      cards.count(_ match {case Card(_, Spot(_)) => true case _ => false})

    def amountOfFace(face: Rank) = {
      require(List(Jack, Queen, King).contains(face))
      cards.count(_.rank == face)
    }

    val amountWithFace: Int =
      cards.length - amountWithNumerical

    val firstCard =
      cards.head

    val allCards =
      cards
  }

  object Deck {
    def apply() = {
      new Deck(scala.util.Random.shuffle(SortedStandardCardsSet()))
    }
  }

  object SortedStandardCardsSet {
    val colors = List(Clubs, Diamonds, Hearts, Spades)
    val ranks = List(Jack, Queen, King, Ace) ++ (for (n <- 2 to 10) yield Spot(n))
    def apply() =
      (for (c <- colors; r <- ranks) yield Card(c, r)).toList
  }
}

package games {
  import cards._
  import deck._

  class Blackjack(deck: Deck) {
    def play(n: Int): Unit = {
      def doPlay(deck: Deck, points: Int, n: Int): Int = {
        if (n == 0) points else {
          val newCard = humanReadableCard(deck.firstCard)
          val newPoints = points + cardValue(deck.firstCard, points)

          println(s"Got $newCard, current points: $newPoints")
          doPlay(deck.pull(), newPoints, n-1)
        }
      }

      val totalPoints = doPlay(deck, 0, n)
      val result = if (totalPoints == 21) "perfect!" else if (totalPoints > 21) "a bit too much" else "not enough"
      println(s"You got $totalPoints points total, and that's $result")
    }

    private def humanReadableCard(card: Card) = {
      (card.color match {
        case Clubs => "♣"
        case Diamonds => "♦"
        case Hearts => "♥"
        case Spades => "♠"
      }) + (card.rank match {
        case Spot(n) => n.toString
        case Jack => "J"
        case Queen => "Q"
        case King => "K"
        case Ace => "A"
      })
    }

    private def cardValue(card: Card, pointsSoFar: Int) =
      card match {
        case Card(_, Spot(n)) => n
        case Card(_, Ace) => if (pointsSoFar <= 10) 11 else 1
        case _ => 10
    }

    private def gameValue(cards: List[Card]) =
      cards.foldLeft(0)((points, card) => points + cardValue(card, points))

    lazy val all21: List[List[Card]] =
      (for {prefix <- deck.allCards.inits.toList; substring <- prefix.tails} yield substring.toList).filter(gameValue(_) == 21).toList

    def first21(): Unit = {
      def keepTrying(cards: List[Card], points: Int, acc: List[Card]): List[Card] = {
        if (points == 21)
          acc
        else if (points < 21)
          keepTrying(cards.tail, points+cardValue(cards.head, points), cards.head :: acc)
        else
          acc.tails.filter(gameValue(_) == 21).toList match {
            case h :: t => h
            case _ => keepTrying(cards.tail, 0, Nil)
          }
      }

      val first = keepTrying(deck.allCards, 0, Nil)
      println("First winning sequence for this deck would be:")
      first.map((c) => println(humanReadableCard(c)))
    }
  }

  object Blackjack {
    def apply(numOfDecks: Int) =
      new Blackjack(MultiDeck(numOfDecks))
  }

  object MultiDeck {
    def apply(cardSetsCount: Int) = {
      new Deck(
        scala.util.Random.shuffle((for (_ <- 1 to cardSetsCount) yield SortedStandardCardsSet()).flatten.toList))
    }
  }
}

object Lab04 {
  def main(args: Array[String]) = {
    import cards._
    import deck._
    import games._

    val d = new deck.Deck(List(Card(Hearts, Queen), Card(Diamonds, Ace)))

    println(Card(Diamonds, Ace) == d.pull().firstCard)
    println(d.push(Card(Hearts, Spot(9))).firstCard == Card(Hearts, Spot(9)))
    println(d.firstCard == d.push(Card(Diamonds, Spot(2))).pull().firstCard)
    println(Deck().isStandard)
    println(0 == Deck().duplicatesOfCard(Card(Spades, Ace)))
    println(1 == Deck().push(Card(Spades, Ace)).duplicatesOfCard(Card(Spades, Ace)))
    println(52/4 == Deck().amountOfColor(Clubs))
    println(52/13 == Deck().amountOfFace(Jack))
    println(9*4 == Deck().amountWithNumerical)

    Blackjack(3).play(3)
    println(new Blackjack(Deck()).all21.length)
    Blackjack(1).first21()
  }
}
