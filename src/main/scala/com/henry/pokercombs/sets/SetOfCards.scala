package com.henry.pokercombs.sets

import com.henry.pokercombs.cards.{Card, Rank, Suit}

case class SetOfCards(val cards: Set[Card]) {
  private lazy val rankMap: Map[Rank, Set[Card]] = cards.groupBy(_.rank)
  private lazy val suitMap: Map[Suit, Set[Card]] = cards.groupBy(_.suit)

  def uniqueRanks: Set[Rank] = rankMap.keySet

  def uniqueSuits: Set[Suit] = suitMap.keySet

  def getSameRankCards(rank: Rank): Set[Card] = rankMap.getOrElse(rank, Set())

  def getSameSuitCards(suit: Suit): Set[Card] = suitMap.getOrElse(suit, Set())

  def excludeCards(excludedCards: Set[Card]): SetOfCards = SetOfCards(cards.diff(excludedCards))

  def excludeRanks(excludedRanks: Set[Rank]): SetOfCards =
    SetOfCards(cards.filterNot(card => excludedRanks.contains(card.rank)))
}
