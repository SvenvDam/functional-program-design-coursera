package quickcheck

import org.scalacheck._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] =
    oneOf(
      const(empty),
      for {
        int <- arbitrary[Int]
        heap <- genHeap
      } yield insert(int, heap))

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min of two") = forAll { _: H =>
    val elem1 = 1
    val elem2 = 2
    val heap = insertMany(empty, elem1, elem2)
    findMin(heap) == 1
  }

  property("insert + delete idempotence") = forAll { _: H =>
    val elem = 1
    val heap = single(elem)
    val deleted = deleteMin(heap)
    isEmpty(deleted)
  }

  property("minimum preserved after meld") = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    (h1, h2) match {

      case (l, r) if isEmpty(l) & isEmpty(r) =>
        true

      case (l, r) if isEmpty(l) =>
        findMin(melded) == findMin(r)

      case (l, r) if isEmpty(r) =>
        findMin(melded) == findMin(l)

      case _ =>
        val min1 = findMin(h1)
        val min2 = findMin(h2)
        scala.math.min(min1, min2) == findMin(melded)
    }
  }

  property("meld preserves all elements")  = forAll { (h1: H, h2: H) =>
    val melded = meld(h1, h2)
    elemSet(h1) ++ elemSet(h2) == elemSet(melded)
  }

  def elemSet(h: H,acc: Set[A] = Set.empty): Set[A] =
    if (isEmpty(h)) acc
    else elemSet(deleteMin(h), acc + findMin(h))
}
