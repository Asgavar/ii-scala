package deck {
  import cards._

  class Deck(cards: List[Card]) {
    def pull() =
      new Deck(cards.drop(1))

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
      cards.count(_ match { case Card(_, Spot(_)) => true case _ => false })

    def amountOfFace(face: Rank) = {
      require(List(Jack, Queen, King, Ace).contains(face))
      cards.count(_.rank == face)
    }

    val amountWithFace: Int =
      cards.length - amountWithNumerical

    val firstCard =
      cards.headOption

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
