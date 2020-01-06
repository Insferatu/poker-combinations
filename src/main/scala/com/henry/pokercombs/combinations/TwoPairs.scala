package com.henry.pokercombs.combinations

import cats.{Order, Show}
import com.henry.pokercombs.cards.{Card, Rank, Suit}
import com.henry.pokercombs.sets.SetOfCards

case class TwoPairs(firstPairRank: Rank,
                    firstPairSuits: Set[Suit],
                    secondPairRank: Rank,
                    secondPairSuits: Set[Suit],
                    fifthCard: Card) extends Combination {
  override val weight = 3
}

object TwoPairs {
  def bestFrom(setOfCards: SetOfCards): Option[TwoPairs] = {
    import cats.instances.order._
    import com.henry.pokercombs.cards.CardOrder._
    import com.henry.pokercombs.combinations.TwoPairsOrderInstances._

    val allTwoPairs = for {
      checkedRanks <- setOfCards.uniqueRanks.toList.sorted(Ordering[Rank].reverse).inits.toList.tail.reverse.tail
      firstPairRank = checkedRanks.last
      firstPair <- setOfCards.getSameRankCards(firstPairRank).toList.combinations(2).map(_.toSet)
      firstPairSuits = firstPair.map(_.suit)
      reducedSetOfCards = setOfCards.excludeRanks(checkedRanks.toSet)
      secondPairRank <- reducedSetOfCards.uniqueRanks.toList.sorted(Ordering[Rank].reverse)
      secondPair <- reducedSetOfCards.getSameRankCards(secondPairRank).toList.combinations(2).map(_.toSet)
      secondPairSuits = secondPair.map(_.suit)
      fifthCard <- setOfCards.excludeCards(firstPair ++ secondPair)
        .cards
        .toList
        .maxOption
    } yield TwoPairs(firstPairRank, firstPairSuits, secondPairRank, secondPairSuits, fifthCard)
    allTwoPairs.maxOption
  }
}

object TwoPairsOrderInstances {
  import cats.instances.tuple._
  import com.henry.pokercombs.cards.CardOrder._

  implicit val twoPairsOrder: Order[TwoPairs] =
    Order.by {
      case TwoPairs(firstPairRank, _, secondPairRank, _, fifthCard) => (firstPairRank, secondPairRank, fifthCard)
    }
}

object TwoPairsShowInstances {
  import cats.syntax.show._
  import cats.instances.string._

  implicit def twoPairsShow(implicit rs: Show[Rank], ss: Show[Suit], cs: Show[Card]): Show[TwoPairs] = {
    case TwoPairs(firstPairRank, firstPairSuits, secondPairRank, secondPairSuits, fifthCard) =>
      val firstPairSuitsStr = firstPairSuits.map(_.show).mkString(", ")
      val secondPairSuitsStr = secondPairSuits.map(_.show).mkString(", ")
      show"""Two pairs: ${firstPairRank}\u27152 (${firstPairSuitsStr}),
            | ${secondPairRank}\u27152 (${secondPairSuitsStr}),
            | ${fifthCard}""".stripMargin.replace("\n", "")
  }
}
