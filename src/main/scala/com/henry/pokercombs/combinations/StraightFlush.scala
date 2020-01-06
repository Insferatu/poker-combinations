package com.henry.pokercombs.combinations

import cats.{Order, Show}
import com.henry.pokercombs.cards.{Card, Rank, Suit, Two}
import com.henry.pokercombs.sets.SetOfCards

case class StraightFlush(firstCard: Card,
                         secondCard: Card,
                         thirdCard: Card,
                         forthCard: Card,
                         fifthCard: Card) extends Combination {
  override val weight = 9
}

object StraightFlush {
  def bestFrom(setOfCards: SetOfCards): Option[StraightFlush] = {
    import cats.syntax.eq._
    import cats.instances.order._
    import com.henry.pokercombs.cards.CardOrder._
    import com.henry.pokercombs.combinations.StraightFlushOrderInstances._

    val uniqueRanks = setOfCards.uniqueRanks
    val allStraightFlushes = for {
      fifthRank <- uniqueRanks.toList.sorted if uniqueRanks.size >= 5
      fifthCard <- setOfCards.getSameRankCards(fifthRank)
      suit = fifthCard.suit
      forthRank = fifthRank.nextRank.getOrElse(Two)
      forthCard <- setOfCards.getSameRankCards(forthRank).filter(_.suit === suit)
      thirdRank <- forthRank.nextRank.toList
      thirdCard <- setOfCards.getSameRankCards(thirdRank).filter(_.suit === suit)
      secondRank <- thirdRank.nextRank.toList
      secondCard <- setOfCards.getSameRankCards(secondRank).filter(_.suit === suit)
      firstRank <- secondRank.nextRank.toList
      firstCard <- setOfCards.getSameRankCards(firstRank).filter(_.suit === suit)
    } yield StraightFlush(firstCard, secondCard, thirdCard, forthCard, fifthCard)
    allStraightFlushes.maxOption
  }
}

object StraightFlushOrderInstances {
  import com.henry.pokercombs.cards.CardOrder._

  implicit val straightFlushOrder: Order[StraightFlush] = Order.by(_.firstCard.rank)
}

object StraightFlushShowInstances {
  import cats.syntax.show._

  implicit def straightFlushShow(implicit rs: Show[Rank], ss: Show[Suit], cs: Show[Card]): Show[StraightFlush] = {
    case StraightFlush(firstCard, secondCard, thirdCard, forthCard, fifthCard) =>
      show"Straight flush: ${firstCard} - ${secondCard} - ${thirdCard} - ${forthCard} - ${fifthCard}"
  }
}
