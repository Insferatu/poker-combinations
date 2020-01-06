package com.henry.pokercombs.combinations

import cats.{Order, Show}
import com.henry.pokercombs.cards.{Card, Rank, Suit}
import com.henry.pokercombs.sets.SetOfCards

case class Flush(suit: Suit, ranks: List[Rank]) extends Combination {
  override val weight = 6
}

object Flush {
  def bestFrom(setOfCards: SetOfCards): Option[Flush] = {
    import cats.instances.order._
    import com.henry.pokercombs.cards.CardOrder._
    import com.henry.pokercombs.combinations.FlushOrderInstances._

    val allFlushes = for {
      suit <- setOfCards.uniqueSuits
      flushCards <- setOfCards.getSameSuitCards(suit).toList.combinations(5)
      ranks = flushCards.map(_.rank).sorted(Ordering[Rank].reverse)
    } yield Flush(suit, ranks)
    allFlushes.maxOption
  }
}

object FlushOrderInstances {
  import com.henry.pokercombs.cards.CardOrder._
  import cats.instances.list._

  implicit val flushOrder: Order[Flush] = Order.by(_.ranks)
}

object FlushShowInstances {
  import cats.syntax.show._
  import cats.instances.string._

  implicit def flushShow(implicit rs: Show[Rank], ss: Show[Suit], cs: Show[Card]): Show[Flush] = {
    case Flush(suit, ranks) =>
      val ranksStr = ranks.map(_.show).mkString(", ")
      show"Flush: ${suit} (${ranksStr})"
  }
}
