package com.henry.pokercombs.combinations

import cats.{Order, Show}
import com.henry.pokercombs.cards.{Card, Rank, Suit}
import com.henry.pokercombs.sets.SetOfCards

case class Pair(pairRank: Rank,
                pairSuits: Set[Suit],
                thirdCard: Card,
                forthCard: Card,
                fifthCard: Card) extends Combination {
  override val weight = 2
}

object Pair {
  def apply(pairRank: Rank, pairSuits: Set[Suit], cards: List[Card]): Option[Pair] = {
    import cats.syntax.option._

    cards match {
      case List(thirdCard, forthCard, fifthCard) => Pair(pairRank, pairSuits, thirdCard, forthCard, fifthCard).some
      case _ => None
    }
  }

  def bestFrom(setOfCards: SetOfCards): Option[Pair] = {
    import cats.instances.order._
    import com.henry.pokercombs.cards.CardOrder._
    import com.henry.pokercombs.combinations.PairOrderInstances._

    val allPairs = for {
      card <- setOfCards.cards
      pairRank = card.rank
      pairCards <- setOfCards.getSameRankCards(pairRank).toList.combinations(2).map(_.toSet)
      pairSuits = pairCards.map(_.suit)
      restOfTheCards = setOfCards.excludeCards(pairCards)
        .cards
        .toList
        .sorted(Ordering[Card].reverse)
        .take(3)
      pair <- Pair(pairRank, pairSuits, restOfTheCards)
    } yield pair
    allPairs.maxOption
  }
}

object PairOrderInstances {
  import cats.instances.tuple._
  import com.henry.pokercombs.cards.CardOrder._

  implicit val pairOrder: Order[Pair] =
    Order.by { case Pair(pairRank, _, thirdCard, forthCard, fifthCard) => (pairRank, thirdCard, forthCard, fifthCard) }
}

object PairShowInstances {
  import cats.syntax.show._
  import cats.instances.string._

  implicit def pairShow(implicit rs: Show[Rank], ss: Show[Suit], cs: Show[Card]): Show[Pair] = {
    case Pair(pairRank, pairSuits, thirdCard, forthCard, fifthCard) =>
      val suitsStr = pairSuits.map(_.show).mkString(", ")
      show"Pair: ${pairRank}\u27152 (${suitsStr}), ${thirdCard}, ${forthCard}, ${fifthCard}"
  }
}
