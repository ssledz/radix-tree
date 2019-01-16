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
  }

  test("testPut") {

  }

  test("testGet") {

    val trie = RadixTree("cute" -> 1, "nice" -> 2, "cuter" -> 3, "tester" -> 4, "test" -> 5)
    println(trie)
    assert(trie.get("cute").sorted === List(1, 3))
    assert(trie.get("cuter") contains 3)
    assert(trie.get("nice") contains 2)
    assert(trie.get("test").sorted == List(4, 5))
    assert(trie.get("tester") contains 4)
    assert(trie.get("tes").isEmpty)
    assert(trie.get("te").isEmpty)


    val trie2 = RadixTree("test" -> 5, "team" -> 6)
    println(trie2)
    assert(trie2.get("team") contains 6)
    assert(trie2.get("test") contains 5)
  }

}