package pl.softech.collection

import org.scalatest.FunSuite
import pl.softech.collection.RadixTree.commonPrefix

class RadixTreeTest extends FunSuite {

  test("commonPrefix") {
    assert(commonPrefix("test", "tester") === Some((Some("test"), None, None)))
    assert(commonPrefix("tester", "test") === Some((None, Some("test"), None)))
    assert(commonPrefix("tester", "team") === Some((None, None, Some("te"))))
    assert(commonPrefix("t", "test") === Some((Some("t"), None, None)))
    assert(commonPrefix("cute", "test") === None)
    assert(commonPrefix("cute", "cuter") === Some((Some("cute"), None, None)))
    assert(commonPrefix("cute", "cute") === Some((Some("cute"), Some("cute"), None)))
  }

  test("put") {
    val trie = RadixTree[Int]()
    assert(trie.put("cute", 1).getPrefixed("cute") contains 1)
    assert(trie.put("cute", 1).put("cute", 2).getPrefixed("cute") contains 2)
    assert(trie.put("", 2).getPrefixed("") contains 2)
    assert(trie.put("", 2).getPrefixed("x") isEmpty)
  }

  test("startsWith") {
    assert(RadixTree.empty.startsWith(""))
    assert(!RadixTree.empty.startsWith("cute"))
    assert(RadixTree("int_one" -> 1).startsWith("int_"))
    assert(!RadixTree("one" -> 1).startsWith("int_"))
  }

  test("contains") {
    val trie = RadixTree("moto.wp.pl" -> (), "sport.wp.pl" -> (), "onet.pl" -> ())
    assert(trie.contains("wp.pl") === false)
    assert(trie.contains("sport.wp.pl") === true)
    assert(trie.contains("onet.pl") === true)
  }

  test("get") {
    val trie = RadixTree("moto.wp.pl" -> 1, "sport.wp.pl" -> 2, "onet.pl" -> 3)
    assert(trie.get("wp.pl") === None)
    assert(trie.get("sport.wp.pl") === Some(2))
    assert(trie.get("onet.pl") === Some(3))
  }

  test("getPrefixed") {
    val trie = RadixTree("cute" -> 1, "nice" -> 2, "cuter" -> 3, "tester" -> 4, "test" -> 5, "team" -> 6)
    assert(trie.getPrefixed("cute").sorted === List(1, 3))
    assert(trie.getPrefixed("cuter") contains 3)
    assert(trie.getPrefixed("nice") contains 2)
    assert(trie.getPrefixed("test").sorted == List(4, 5))
    assert(trie.getPrefixed("tes").sorted == List(4, 5))
    assert(trie.getPrefixed("te").sorted == List(4, 5, 6))
    assert(trie.getPrefixed("t").sorted == List(4, 5, 6))
    assert(trie.getPrefixed("tester") contains 4)
    assert(trie.getPrefixed("x").isEmpty)
    assert(trie.getPrefixed("").sorted == List(1, 2, 3, 4, 5, 6))

    val trie2 = RadixTree("test" -> 5, "team" -> 6)
    assert(trie2.getPrefixed("team") contains 6)
    assert(trie2.getPrefixed("test") contains 5)

    val trie3 = trie2.put("toast", 7)
    assert(trie2.getPrefixed("toast").isEmpty)
    assert(trie3.getPrefixed("toast") contains 7)
  }

}
