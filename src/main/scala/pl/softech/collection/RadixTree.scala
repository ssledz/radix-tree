package pl.softech.collection

import pl.softech.collection.RadixTree.{commonPrefix, find}

import scala.annotation.tailrec

sealed trait RadixTree[+A] {
  self =>

  def put[B >: A](key: String, value: B): RadixTree[B] = self match {

    case Branch(Nil) => Branch(List(Edge(key, Leaf(value))))

    case Branch(rootEdges) => {

      val edge = find(rootEdges)(e => commonPrefix(key, e.label).map((e, _)))

      edge match {

        case Some((e, (_, Some(prefix), None))) => {
          val newNode = e.node.put(key.substring(prefix.length), value)
          val newEdge = e.copy(node = newNode)
          Branch(newEdge :: rootEdges.filterNot(_ == e))
        }

        case Some((e, (Some(prefix), _, None))) => {
          e.node match {
            case leaf@Leaf(_) => {
              val newLabel = e.label.substring(prefix.length)
              val newEdge: Edge[B] = Edge(key, Branch(List(Edge(newLabel, leaf), Edge("", Leaf(value)))))
              Branch(newEdge :: rootEdges.filterNot(_ == e))
            }
          }
        }

        case Some((e, (None, None, Some(prefix)))) => {
          e.node match {
            case leaf@Leaf(_) => {
              val le = Edge(e.label.substring(prefix.length), leaf)
              val re = Edge(key.substring(prefix.length), Leaf(value))
              val newEdge: Edge[B] = Edge(prefix, Branch(List(le, re)))
              Branch(newEdge :: rootEdges.filterNot(_ == e))
            }
            case Branch(childEdges) => {
              val le = Edge(e.label.substring(prefix.length), Branch(childEdges))
              val re = Edge(key.substring(prefix.length), Leaf(value))
              val newEdge = Edge(prefix, Branch(List(le, re)))
              Branch(newEdge :: rootEdges.filterNot(_ == e))
            }
          }
        }

        case None => Branch(Edge(key, Leaf(value)) :: rootEdges)
      }
    }

    case leaf@Leaf(_) if key.nonEmpty => Branch(List(Edge("", leaf), Edge(key, Leaf(value))))
  }

  def values: List[A] = {

    @tailrec
    def iter(xs: List[RadixTree[A]], acc: List[A]): List[A] = xs match {
      case Leaf(value) :: tail => iter(tail, value :: acc)
      case Branch(es) :: tail => iter(es.map(_.node) ::: tail, acc)
      case Nil => acc
    }

    iter(List(self), List.empty)
  }

  def get(key: String): List[A] = (key.isEmpty, self) match {
    case (true, Leaf(value)) => List(value)
    case (true, _) => values
    case (false, Branch(xs)) => {
      val edge = find(xs)(e => if (e.label.nonEmpty) commonPrefix(key, e.label).map((e, _)) else None)
      edge match {
        case Some((e, (None, Some(prefix), None))) => e.node.get(key.substring(prefix.length))
        case Some((e, (Some(_), _, None))) => e.node.get("")
        case _ => List.empty
      }
    }
    case _ => List.empty
  }

}

case class Edge[+A](label: String, node: RadixTree[A])

case class Branch[A](edges: List[Edge[A]]) extends RadixTree[A]

case class Leaf[A](value: A) extends RadixTree[A]

object RadixTree {

  def empty[A]: RadixTree[A] = Branch(List.empty)

  def apply[A](xs: (String, A)*): RadixTree[A] = xs.foldLeft(empty[A]) {
    case (acc, (key, value)) => acc.put(key, value)
  }

  def find[A, B](xs: List[A])(f: A => Option[B]): Option[B] = {
    var curr = xs
    while (!curr.isEmpty) {
      f(curr.head) match {
        case v@Some(_) => return v
        case _ =>
      }
      curr = curr.tail
    }
    None
  }

  def commonPrefix(x: String, y: String): Option[(Option[String], Option[String], Option[String])] = {

    @tailrec
    def go(x: String, y: String, prefix: String): Option[(Option[String], Option[String], Option[String])] =
      (x.headOption, y.headOption) match {
        case (Some(cx), Some(cy)) if cx == cy => go(x.tail, y.tail, prefix + cx)
        case (Some(_), Some(_)) if prefix.nonEmpty => Some((None, None, Some(prefix)))
        case (None, Some(_)) if prefix.nonEmpty => Some((Some(prefix), None, None))
        case (Some(_), None) if prefix.nonEmpty => Some((None, Some(prefix), None))
        case (None, None) if prefix.nonEmpty => Some((Some(prefix), Some(prefix), None))
        case _ => None
      }

    go(x, y, "")

  }

}