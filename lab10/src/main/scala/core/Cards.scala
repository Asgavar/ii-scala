package cards {
  abstract class Color
  abstract class Rank

  case class Card(color: Color, rank: Rank) {
    def toHumanReadable(): String = {
      (color match {
        case Clubs => "♣"
        case Diamonds => "♦"
        case Hearts => "♥"
        case Spades => "♠"
      }) + (rank match {
        case Spot(n) => n.toString
        case Jack => "J"
        case Queen => "Q"
        case King => "K"
        case Ace => "A"
      })
    }
  }

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
