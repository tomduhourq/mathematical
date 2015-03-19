package scalacheck

import org.scalacheck._
import Prop.forAll

object ListSpecification extends Properties("List") {

  property("double reverse") = forAll { (lst: List[Int]) =>
    lst.reverse.reverse == lst
  }

  property("zip reverse") = forAll { (a: List[Int], b: List[Int]) =>
    // Ensuring that both lists have the same length
    val a1 = a.take(b.length)
    val b1 = b.take(a.length)
    // This property always fails if we have distinct lengths
    (a1.reverse zip b1.reverse) == (a1 zip b1).reverse
  }

  property("concatenate") = forAll { (l1: List[Int], l2: List[Int]) =>
    l1.size + l2.size == (l1 ::: l2).size
  }
}
