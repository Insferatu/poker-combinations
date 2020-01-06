package com.henry.pokercombs.combinations

import cats.{Order, Show}
import com.henry.pokercombs.cards.{Card, Rank, Suit}
import com.henry.pokercombs.sets.SetOfCards

case class FullHouse(firstTripleRank: Rank,
                     firstTripleSuits: Set[Suit],
                     secondPairRank: Rank,
                     secondPairSuits: Set[Suit]) extends Combination {
  override val weight = 7
}

object FullHouse {
  def bestFrom(setOfCards: SetOfCards): Option[FullHouse] = {
    import cats.instances.order._
    import com.henry.pokercombs.cards.CardOrder._
    import com.henry.pokercombs.combinations.FullHouseOrderInstances._

    val allFullHouses = for {
      firstTripleRank <- setOfCards.uniqueRanks.toList.sorted(Ordering[Rank].reverse)
      firstTriple <- setOfCards.getSameRankCards(firstTripleRank).toList.combinations(3).map(_.toSet)
      firstTripleSuits = firstTriple.map(_.suit)
      reducedSetOfCards = setOfCards.excludeCards(firstTriple)
      secondPairRank <- reducedSetOfCards.uniqueRanks.toList.sorted(Ordering[Rank].reverse)
      secondPair <- reducedSetOfCards.getSameRankCards(secondPairRank).toList.combinations(2).map(_.toSet)
      secondPairSuits = secondPair.map(_.suit)
    } yield FullHouse(firstTripleRank, firstTripleSuits, secondPairRank, secondPairSuits)
    allFullHouses.maxOption
  }
}

object FullHouseOrderInstances {
  import cats.instances.tuple._
  import com.henry.pokercombs.cards.CardOrder._

  implicit val fullHouseOrder: Order[FullHouse] =
    Order.by {
      case FullHouse(firstPairRank, _, secondPairRank, _) => (firstPairRank, secondPairRank)
    }
}

object FullHouseShowInstances {
  import cats.syntax.show._
  import cats.instances.string._

  implicit def fullHouseShow(implicit rs: Show[Rank], ss: Show[Suit], cs: Show[Card]): Show[FullHouse] = {
    case FullHouse(firstTripleRank, firstTripleSuits, secondPairRank, secondPairSuits) =>
      val firstTripleSuitsStr = firstTripleSuits.map(_.show).mkString(", ")
      val secondPairSuitsStr = secondPairSuits.map(_.show).mkString(", ")
      show"""Full house: ${firstTripleRank}\u27153 (${firstTripleSuitsStr}),
            | ${secondPairRank}\u27152 (${secondPairSuitsStr})""".stripMargin.replace("\n", "")
  }
}
