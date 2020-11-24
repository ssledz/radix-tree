package pl.softech.collection

import org.scalameter.Context
import org.scalameter.api.{Bench, _}
import org.scalameter.picklers.Implicits._

import scala.io.Source

object RadixTreeBenchmark extends Bench.LocalTime {

  val tags: List[String] = Source.fromInputStream(getClass.getResourceAsStream("/words.txt")).getLines().toList

  val opts = Context(
    exec.benchRuns -> 100,
    exec.minWarmupRuns -> 100
  )

  val allPrefixes: List[String] = for {
    index <- (0 until tags.size).toList
    tag = tags(index)
    tagLen <- 1 until tag.length
  } yield tag.substring(0, tagLen)

  val gen = for {
    pre <- Gen.enumeration("pre")("", "x")
    post <- Gen.enumeration("post")("", "x")
  } yield allPrefixes.map(p => pre + p + post)

  performance of "pl.softech.collection.RadixTree" config opts in {
    val tree = RadixTree(tags.map(t => (t, t)): _*)
    measure method "get" in {
      using(gen) in { prefixes =>
        prefixes.foreach(tree.getPrefixed(_))
      }
    }

    measure method "startsWith" in {
      using(gen) in { prefixes =>
        prefixes.foreach(tree.startsWith(_))
      }
    }
  }

  performance of "scala.collection.immutable.Set" config opts in {
    val set = Set(tags.map(t => (t, t)): _*)
    measure method "get" in {
      using(gen) in { prefixes =>
        prefixes.foreach(prefix => set.filter(t => t._1.startsWith(prefix)))
      }
    }

    measure method "startsWith" in {
      using(gen) in { prefixes =>
        prefixes.foreach(prefix => set.exists(t => t._1.startsWith(prefix)))
      }
    }
  }


}
