package com.henry.pokercombs.combinations

import cats.{Order, Show}
import com.henry.pokercombs.cards.{Card, Rank, Suit, Two}
import com.henry.pokercombs.sets.SetOfCards

case class Straight(firstCard: Card,
                    secondCard: Card,
                    thirdCard: Card,
                    forthCard: Card,
                    fifthCard: Card) extends Combination {
  override val weight = 5
}

object Straight {
  def bestFrom(setOfCards: SetOfCards): Option[Straight] = {
    import cats.instances.order._
    import com.henry.pokercombs.cards.CardOrder._
    import com.henry.pokercombs.combinations.StraightOrderInstances._

    val uniqueRanks = setOfCards.uniqueRanks
    val allStraights = for {
      fifthRank <- uniqueRanks.toList.sorted if uniqueRanks.size >= 5
      fifthCard <- setOfCards.getSameRankCards(fifthRank)
      forthRank = fifthRank.nextRank.getOrElse(Two)
      forthCard <- setOfCards.getSameRankCards(forthRank)
      thirdRank <- forthRank.nextRank.toList
      thirdCard <- setOfCards.getSameRankCards(thirdRank)
      secondRank <- thirdRank.nextRank.toList
      secondCard <- setOfCards.getSameRankCards(secondRank)
      firstRank <- secondRank.nextRank.toList
      firstCard <- setOfCards.getSameRankCards(firstRank)
    } yield Straight(firstCard, secondCard, thirdCard, forthCard, fifthCard)
    allStraights.maxOption
  }
}

object StraightOrderInstances {
  import com.henry.pokercombs.cards.CardOrder._

  implicit val straightOrder: Order[Straight] = Order.by(_.firstCard.rank)
}

object StraightShowInstances {
  import cats.syntax.show._

  implicit def straightShow(implicit rs: Show[Rank], ss: Show[Suit], cs: Show[Card]): Show[Straight] = {
    case Straight(firstCard, secondCard, thirdCard, forthCard, fifthCard) =>
      show"Straight: ${firstCard} - ${secondCard} - ${thirdCard} - ${forthCard} - ${fifthCard}"
  }
}
