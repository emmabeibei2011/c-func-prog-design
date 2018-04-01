package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  // Insert one random element to empty
  property("gen2") = forAll { (a: Int) =>
    findMin(insert(a, empty)) == a
  }


  // Insert two random elements to empty
  property("gen3") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }


  // Insert one random to empty and then delete
  property("gen3") = forAll { (a: Int) =>
    isEmpty(deleteMin(insert(a, empty)))
  }


  // Current findMin should be <= last findMin()
  property("gen4") = forAll { (h: H) =>
    def dfs(h: H, lastMin: Int): Boolean = {
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        lastMin <= m && dfs(deleteMin(h), m)
      }
    }
    dfs(h, Int.MinValue)
  }

  // The new min of melted list should be the smaller of min(h1) and min(h2)
  property("gen5") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) && isEmpty((h2))) meld(h1, h2) == empty
    else if (isEmpty(h1)) meld(h1, h2) == h2
    else if (isEmpty(h2)) meld(h1, h2) == h1
    else {
      val m = findMin(meld(h1, h2))
      findMin(h1) == m || findMin(h2) == m
    }
  }


  // melding two lists should be the same if you move elements from h1 to h2
  property("gen6") = forAll { (h1: H, h2: H) =>
    def minsList(h: H): List[Int] = {
      if (isEmpty(h)) Nil
      else findMin(h) :: minsList(deleteMin(h))
    }
    if (!isEmpty(h1) && !isEmpty(h2)) {
      val ml1 = minsList(meld(h1, h2))
      val min1 = findMin(h1)
      val meld2 = meld(deleteMin(h1), insert(min1, h2))
      val ml2 = minsList(meld2)
      ml1 == ml2
    } else true


  }
}
