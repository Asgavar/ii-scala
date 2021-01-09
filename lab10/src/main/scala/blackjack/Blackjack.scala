package blackjack {
  import cards._
  import deck._

  class Blackjack(deck: Deck) {
    def play(n: Int): Unit = {
      def doPlay(deck: Deck, points: Int, n: Int): Int = {
        if (n == 0) points else deck.firstCard match {
          case None => points
          case Some(firstCard) => {
            val newCard = firstCard.toHumanReadable()
            val newPoints = points + cardValue(firstCard, points)

            println(s"Got $newCard, current points: $newPoints")
            doPlay(deck.pull(), newPoints, n - 1)
          }
        }
      }

      val totalPoints = doPlay(deck, 0, n)
      val result = if (totalPoints == 21) "perfect!" else if (totalPoints > 21) "a bit too much" else "not enough"
      println(s"You got $totalPoints points total, and that's $result")
    }

    def cardValue(card: Card, pointsSoFar: Int) =
      card match {
        case Card(_, Spot(n)) => n
        case Card(_, Ace) => if (pointsSoFar <= 10) 11 else 1
        case _ => 10
      }

    def gameValue(cards: List[Card]) =
      cards.foldLeft(0)((points, card) => points + cardValue(card, points))

    lazy val all21: List[List[Card]] =
      (for { prefix <- deck.allCards.inits.toList; substring <- prefix.tails } yield substring.toList).filter(gameValue(_) == 21).toList

    def first21(): List[Card] = {
      def keepTrying(cards: List[Card], points: Int, acc: List[Card]): List[Card] = {
        if (points == 21)
          acc
        else if (points < 21)
          keepTrying(cards.tail, points + cardValue(cards.head, points), cards.head :: acc)
        else
          acc.tails.filter(gameValue(_) == 21).toList match {
            case h :: t => h
            case _ => keepTrying(cards.tail, 0, Nil)
          }
      }

      val first = keepTrying(deck.allCards, 0, Nil)
      println("First winning sequence for this deck would be:")
      first.map((c) => println(c.toHumanReadable()))

      first
    }
  }

  object Blackjack {
    def apply(numOfDecks: Int) =
      new Blackjack(MultiDeck(numOfDecks))
  }

  object MultiDeck {
    def apply(cardSetsCount: Int) = {
      new Deck(
        scala.util.Random.shuffle((for (cardSet <- 1 to cardSetsCount) yield SortedStandardCardsSet()).flatten.toList))
    }
  }
}
