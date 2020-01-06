package com.henry.pokercombs.combinations

import cats.{Order, Show}
import com.henry.pokercombs.cards.{Card, Rank, Suit}
import com.henry.pokercombs.sets.SetOfCards

case class ThreeOfAKind(tripleRank: Rank,
                        tripleSuits: Set[Suit],
                        forthCard: Card,
                        fifthCard: Card) extends Combination {
  override val weight = 4
}

object ThreeOfAKind {
  def apply(tripleRank: Rank, tripleSuits: Set[Suit], cards: List[Card]): Option[ThreeOfAKind] = {
    import cats.syntax.option._

    cards match {
      case List(forthCard, fifthCard) => ThreeOfAKind(tripleRank, tripleSuits, forthCard, fifthCard).some
      case _ => None
    }
  }

  def bestFrom(setOfCards: SetOfCards): Option[ThreeOfAKind] = {
    import com.henry.pokercombs.cards.CardOrder._
    import com.henry.pokercombs.combinations.ThreeOfAKindOrderInstances._
    import cats.instances.order._

    val allThreeOfAKinds = for {
      card <- setOfCards.cards
      tripleRank = card.rank
      tripleCards <- setOfCards.getSameRankCards(tripleRank).toList.combinations(3).map(_.toSet)
      tripleSuits = tripleCards.map(_.suit)
      restOfTheCards = setOfCards.excludeCards(tripleCards)
        .cards
        .toList
        .sorted(Ordering[Card].reverse)
        .take(2)
      threeOfAKind <- ThreeOfAKind(tripleRank, tripleSuits, restOfTheCards)
    } yield threeOfAKind
    allThreeOfAKinds.maxOption
  }
}

object ThreeOfAKindOrderInstances {
  import com.henry.pokercombs.cards.CardOrder._
  import cats.instances.tuple._

  implicit val threeOfAKindOrder: Order[ThreeOfAKind] = Order.by {
    case ThreeOfAKind(tripleRank, _, forthCard, fifthCard) => (tripleRank, forthCard, fifthCard)
  }
}

object ThreeOfAKindShowInstances {
  import cats.syntax.show._
  import cats.instances.string._

  implicit def threeOfAKindShow(implicit rs: Show[Rank], ss: Show[Suit], cs: Show[Card]): Show[ThreeOfAKind] = {
    case ThreeOfAKind(tripleRank, tripleSuits, forthCard, fifthCard) =>
      val suitsStr = tripleSuits.map(_.show).mkString(", ")
      show"Three of a kind: ${tripleRank}\u27153 (${suitsStr}), ${forthCard}, ${fifthCard}"
  }
}
