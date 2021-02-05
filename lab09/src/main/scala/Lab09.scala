object Lab09 {
  def main(args: Array[String]): Unit = {
    import cards._
    import deck._
    import blackjack._

    val d = new deck.Deck(List(Card(Hearts, Queen), Card(Diamonds, Ace)))

    println(Some(Card(Diamonds, Ace)) == d.pull().firstCard)
    println(d.push(Card(Hearts, Spot(9))).firstCard == Some(Card(Hearts, Spot(9))))
    println(d.firstCard == d.push(Card(Diamonds, Spot(2))).pull().firstCard)
    println(Deck().isStandard)
    println(0 == Deck().duplicatesOfCard(Card(Spades, Ace)))
    println(1 == Deck().push(Card(Spades, Ace)).duplicatesOfCard(Card(Spades, Ace)))
    println(52 / 4 == Deck().amountOfColor(Clubs))
    println(52 / 13 == Deck().amountOfFace(Jack))
    println(9 * 4 == Deck().amountWithNumerical)

    Blackjack(3).play(3)
    println(new Blackjack(Deck()).all21.length)
    Blackjack(1).first21()
  }
}
