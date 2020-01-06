package com.henry.pokercombs.combinations

import cats.{Order, Show}
import com.henry.pokercombs.cards.{Card, Rank, Suit}
import com.henry.pokercombs.sets.SetOfCards

case class FourOfAKind(quadRank: Rank,
                       quadSuits: Set[Suit],
                       fifthCard: Card) extends Combination {
  override val weight = 8
}

object FourOfAKind {
  def bestFrom(setOfCards: SetOfCards): Option[FourOfAKind] = {
    import com.henry.pokercombs.cards.CardOrder._
    import com.henry.pokercombs.combinations.FourOfAKindOrderInstances._
    import cats.instances.order._

    val allThreeOfAKinds = for {
      quadRank <- setOfCards.uniqueRanks
      quadCards <- setOfCards.getSameRankCards(quadRank).toList.combinations(4).map(_.toSet)
      quadSuits = quadCards.map(_.suit)
      fifthCard <- setOfCards.excludeCards(quadCards)
        .cards
        .toList
        .maxOption
    } yield FourOfAKind(quadRank, quadSuits, fifthCard)
    allThreeOfAKinds.maxOption
  }
}

object FourOfAKindOrderInstances {
  import com.henry.pokercombs.cards.CardOrder._
  import cats.instances.tuple._

  implicit val fourOfAKindOrder: Order[FourOfAKind] = Order.by {
    case FourOfAKind(rank, _, fifthCard) => (rank, fifthCard)
  }
}

object FourOfAKindShowInstances {
  import cats.syntax.show._
  import cats.instances.string._

  implicit def fourOfAKindShow(implicit rs: Show[Rank], ss: Show[Suit], cs: Show[Card]): Show[FourOfAKind] = {
    case FourOfAKind(quadRank, quadSuits, fifthCard) =>
      val quadSuitsStr = quadSuits.map(_.show).mkString(", ")
      show"Four of a kind: ${quadRank}\u27153 (${quadSuitsStr}), ${fifthCard}"
  }
}
