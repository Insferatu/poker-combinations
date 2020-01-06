package com.henry.pokercombs.combinations

import cats.{Order, Show}
import com.henry.pokercombs.cards.{Card, Rank, Suit}
import com.henry.pokercombs.sets.SetOfCards

trait Combination {
  def weight: Int
}

object Combination {
  def bestFrom(setOfCards: SetOfCards): Option[Combination] = {
    List[SetOfCards => Option[Combination]](
      FourOfAKind.bestFrom,
      FullHouse.bestFrom,
      Flush.bestFrom,
      Straight.bestFrom,
      ThreeOfAKind.bestFrom,
      TwoPairs.bestFrom,
      Pair.bestFrom,
      HighCard.bestFrom
    ).iterator.map(_.apply(setOfCards)).find(_.isDefined).flatten
  }
}

object CombinationOrderInstances {
  import com.henry.pokercombs.combinations.HighCardOrderInstances._
  import com.henry.pokercombs.combinations.PairOrderInstances._
  import com.henry.pokercombs.combinations.TwoPairsOrderInstances._
  import com.henry.pokercombs.combinations.ThreeOfAKindOrderInstances._
  import com.henry.pokercombs.combinations.StraightOrderInstances._
  import com.henry.pokercombs.combinations.FlushOrderInstances._
  import com.henry.pokercombs.combinations.FullHouseOrderInstances._
  import com.henry.pokercombs.combinations.FourOfAKindOrderInstances._

  implicit val combinationOrder: Order[Combination] = (lc: Combination, rc: Combination) => (lc, rc) match {
    case (lh: HighCard, rh: HighCard) => Order[HighCard].compare(lh, rh)
    case (lp: Pair, rp: Pair) => Order[Pair].compare(lp, rp)
    case (lt: TwoPairs, rt: TwoPairs) => Order[TwoPairs].compare(lt, rt)
    case (lt: ThreeOfAKind, rt: ThreeOfAKind) => Order[ThreeOfAKind].compare(lt, rt)
    case (ls: Straight, rs: Straight) => Order[Straight].compare(ls, rs)
    case (lf: Flush, rf: Flush) => Order[Flush].compare(lf, rf)
    case (lf: FullHouse, rf: FullHouse) => Order[FullHouse].compare(lf, rf)
    case (lf: FourOfAKind, rf: FourOfAKind) => Order[FourOfAKind].compare(lf, rf)
    case _ => lc.weight - rc.weight
  }
}

object CombinationShowInstances {
  import com.henry.pokercombs.combinations.HighCardShowInstances._
  import com.henry.pokercombs.combinations.PairShowInstances._
  import com.henry.pokercombs.combinations.TwoPairsShowInstances._
  import com.henry.pokercombs.combinations.ThreeOfAKindShowInstances._
  import com.henry.pokercombs.combinations.StraightShowInstances._
  import com.henry.pokercombs.combinations.FlushShowInstances._
  import com.henry.pokercombs.combinations.FullHouseShowInstances._
  import com.henry.pokercombs.combinations.FourOfAKindShowInstances._

  implicit def combinationShow(implicit rs: Show[Rank], ss: Show[Suit], cs: Show[Card]): Show[Combination] = {
    case h: HighCard => Show[HighCard].show(h)
    case p: Pair => Show[Pair].show(p)
    case t: TwoPairs => Show[TwoPairs].show(t)
    case t: ThreeOfAKind => Show[ThreeOfAKind].show(t)
    case s: Straight => Show[Straight].show(s)
    case f: Flush => Show[Flush].show(f)
    case f: FullHouse => Show[FullHouse].show(f)
    case f: FourOfAKind => Show[FourOfAKind].show(f)
  }
}