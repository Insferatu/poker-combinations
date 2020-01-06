package com.henry.pokercombs.combinations

import cats.{Order, Show}
import com.henry.pokercombs.cards.Card
import com.henry.pokercombs.sets.SetOfCards

case class HighCard(firstCard: Card,
                    secondCard: Card,
                    thirdCard: Card,
                    forthCard: Card,
                    fifthCard: Card) extends Combination {
  override val weight = 1
}

object HighCard {
  def apply(cards: List[Card]): Option[HighCard] = {
    import cats.syntax.option._

    cards match {
      case List(firstCard, secondCard, thirdCard, forthCard, fifthCard) =>
        HighCard(firstCard, secondCard, thirdCard, forthCard, fifthCard).some
      case _ => None
    }
  }

  def bestFrom(setOfCards: SetOfCards): Option[HighCard] = {
    import cats.instances.order._
    import com.henry.pokercombs.cards.CardOrder._

    HighCard(setOfCards.cards
      .toList
      .sorted(Ordering[Card].reverse)
      .take(5))
  }
}

object HighCardOrderInstances {
  import cats.instances.tuple._
  import com.henry.pokercombs.cards.CardOrder._

  implicit val highCardOrder: Order[HighCard] = Order.by {
    case HighCard(firstCard, secondCard, thirdCard, forthCard, fifthCard) =>
      (firstCard, secondCard, thirdCard, forthCard, fifthCard)
  }
}

object HighCardShowInstances {
  import cats.syntax.show._

  implicit def highCardShow(implicit cs: Show[Card]): Show[HighCard] = {
    case HighCard(firstCard, secondCard, thirdCard, forthCard, fifthCard) =>
      show"High card: ${firstCard}, ${secondCard}, ${thirdCard}, ${forthCard}, ${fifthCard}"
  }
}
