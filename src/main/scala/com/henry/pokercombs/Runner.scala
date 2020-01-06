package com.henry.pokercombs

import com.henry.pokercombs.cards._
import com.henry.pokercombs.combinations.{Combination, Straight}
import com.henry.pokercombs.sets.SetOfCards

object Runner {
  def main(args: Array[String]): Unit = {
    import cats.syntax.show._
    import cats.instances.order._
    import com.henry.pokercombs.cards.CardShortNameShowInstances._
    import com.henry.pokercombs.combinations.CombinationShowInstances._
    import com.henry.pokercombs.combinations.CombinationOrderInstances._

    val board = Set(
      Eight of Hearts,
      Nine of Hearts,
      Ten of Hearts,
      Nine of Spades,
      Ten of Diamonds
    )
    val hand1 = Set(
      Jack of Spades,
      Queen of Clubs
    )
    val hand2 = Set(
      Nine of Diamonds,
      Nine of Clubs
    )
    val hand3 = Set(
      Eight of Clubs,
      Eight of Spades,
    )
    val hand4 = Set(
      Eight of Diamonds,
      Ten of Spades
    )
    val hand5 = Set(
      Two of Hearts,
      Three of Hearts
    )
    val hand6 = Set(
      Four of Spades,
      Ten of Clubs
    )
    val hand7 = Set(
      Jack of Hearts,
      Queen of Hearts
    )
    val comb1 = Combination.bestFrom(SetOfCards(board ++ hand1)).get
    val comb2 = Combination.bestFrom(SetOfCards(board ++ hand2)).get
    val comb3 = Combination.bestFrom(SetOfCards(board ++ hand3)).get
    val comb4 = Combination.bestFrom(SetOfCards(board ++ hand4)).get
    val comb5 = Combination.bestFrom(SetOfCards(board ++ hand5)).get
    val comb6 = Combination.bestFrom(SetOfCards(board ++ hand6)).get
    val comb7 = Combination.bestFrom(SetOfCards(board ++ hand7)).get
    println(show"Player 1 combination: ${comb1}")
    println(show"Player 2 combination: ${comb2}")
    println(show"Player 3 combination: ${comb3}")
    println(show"Player 4 combination: ${comb4}")
    println(show"Player 5 combination: ${comb5}")
    println(show"Player 6 combination: ${comb6}")
    println(show"Player 7 combination: ${comb7}")
    val orderedCombinations = List(
      comb1,
      comb2,
      comb3,
      comb4,
      comb5,
      comb6,
      comb7
    ).sorted(Ordering[Combination].reverse)
    println("Combinations (descending order):")
    println(orderedCombinations.map(comb => show"\t${comb}").mkString("\n"))
    println(show"Winner combination: ${orderedCombinations.head}")
  }
}
