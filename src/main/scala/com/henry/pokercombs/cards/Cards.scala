package com.henry.pokercombs.cards

import cats.{Order, Show}

import scala.collection.immutable.TreeSet

sealed trait Suit {
  def weight: Int
}
case object Clubs extends Suit {
  override def weight: Int = 1
}
case object Diamonds extends Suit {
  override def weight: Int = 2
}
case object Hearts extends Suit {
  override def weight: Int = 3
}
case object Spades extends Suit {
  override def weight: Int = 4
}

sealed trait Rank {
  import com.henry.pokercombs.cards.Rank._

  def of(suit: Suit): Card = Card(this, suit)
  def weight: Int
  def previousRank: Option[Rank] = orderedRanks.maxBefore(this)
  def nextRank: Option[Rank] = reverseOrderedRanks.maxBefore(this)
}
case object Two extends Rank {
  override val weight = 1
}
case object Three extends Rank {
  override val weight = 2
}
case object Four extends Rank {
  override val weight = 3
}
case object Five extends Rank {
  override val weight = 4
}
case object Six extends Rank {
  override val weight = 5
}
case object Seven extends Rank {
  override val weight = 6
}
case object Eight extends Rank {
  override val weight = 7
}
case object Nine extends Rank {
  override val weight = 8
}
case object Ten extends Rank {
  override val weight = 9
}
case object Jack extends Rank {
  override val weight = 10
}
case object Queen extends Rank {
  override val weight = 11
}
case object King extends Rank {
  override val weight = 12
}
case object Ace extends Rank {
  override val weight = 13
}

object Rank {
  import cats.instances.order._
  import com.henry.pokercombs.cards.CardOrder._

  val orderedRanks: TreeSet[Rank] = TreeSet(
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace
  )

  val reverseOrderedRanks: TreeSet[Rank] = TreeSet.from(orderedRanks)(Ordering[Rank].reverse)
}

case class Card(rank: Rank, suit: Suit)

object CardFullNameShowInstances {
  import cats.syntax.show._

  implicit val rankFullNameShow: Show[Rank] = {
    case Two => "Two"
    case Three => "Three"
    case Four => "Four"
    case Five => "Five"
    case Six => "Six"
    case Seven => "Seven"
    case Eight => "Eight"
    case Nine => "Nine"
    case Ten => "Ten"
    case Jack => "Jack"
    case Queen => "Queen"
    case King => "King"
    case Ace => "Ace"
  }

  implicit val suitFullNameShow: Show[Suit] = {
    case Hearts => "Hearts"
    case Spades => "Spades"
    case Diamonds => "Diamonds"
    case Clubs => "Clubs"
  }

  implicit val cardFullNameShow: Show[Card] = { case Card(rank, suit) => show"${rank} of ${suit}" }
}

object CardShortNameShowInstances {
  import cats.syntax.show._

  implicit val rankShortNameShow: Show[Rank] = {
    case Two => "2"
    case Three => "3"
    case Four => "4"
    case Five => "5"
    case Six => "6"
    case Seven => "7"
    case Eight => "8"
    case Nine => "9"
    case Ten => "10"
    case Jack => "J"
    case Queen => "Q"
    case King => "K"
    case Ace => "A"
  }

  implicit val suitShortNameShow: Show[Suit] = {
    case Hearts => "\u2665"
    case Spades => "\u2660"
    case Diamonds => "\u2666"
    case Clubs => "\u2663"
  }

  implicit val cardShortNameShow: Show[Card] = { case Card(rank, suit) => show"${rank}${suit}" }
}

object CardOrder {
  import cats.instances.int._

  implicit val rankOrder: Order[Rank] = Order.by(_.weight)

  implicit val suitOrder: Order[Suit] = Order.by(_.weight)

  implicit val cardOrder: Order[Card] = Order.by(_.rank)
}