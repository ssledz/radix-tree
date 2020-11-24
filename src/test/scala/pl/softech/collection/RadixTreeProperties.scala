package pl.softech.collection

import org.scalacheck.Prop.BooleanOperators
import org.scalacheck.{Prop, Properties}

object RadixTreeProperties extends Properties("RadixTree") {

  property("startsWith is compliant with get") =
    Prop.forAll {
      (a: String, b: String) =>
        (a != b && b.nonEmpty && a.nonEmpty) ==> {
          val tree = RadixTree((a + b) -> (a + b))
          ("!tree.startsWith(b) == tree.get(b).isEmpty)" |: (!tree.startsWith(b) == tree.getPrefixed(b).isEmpty)) &&
            ("tree.startsWith(a) == tree.get(a).nonEmpty" |: (tree.startsWith(a) == tree.getPrefixed(a).nonEmpty))
        }
    }

  property("startsWith for empty tree returns false for non empty key") = Prop.forAll { key: String =>
    (key.nonEmpty) ==> {
      !RadixTree.empty.startsWith(key)
    }
  }

  property("startsWith for empty tree returns true for empty key") = {
    RadixTree.empty.startsWith("")
  }

  property("get for empty tree returns empty collection") = Prop.forAll { key: String =>
    RadixTree.empty.getPrefixed(key).isEmpty
  }

  property("get with empty string returns all elements") = Prop.forAll { (a: String, b: String, c: String) =>
    (a.nonEmpty && b.nonEmpty && c.nonEmpty) ==> {
      val ret = RadixTree(a -> a, b -> b, c -> c).getPrefixed("")
      ("ret contains a" |: (ret contains a)) &&
        ("ret contains b" |: (ret contains b)) &&
        ("ret contains c" |: (ret contains c))
    }
  }

}
