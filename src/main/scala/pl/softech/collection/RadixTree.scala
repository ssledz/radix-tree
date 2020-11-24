package pl.softech.collection

import pl.softech.collection.RadixTree._

import scala.annotation.tailrec

sealed trait RadixTree[+A] {
  self =>

  def put[B >: A](key: String, value: B): RadixTree[B] = self match {
    case Branch(Nil) => Branch(List(Edge(key, Leaf(value))))
    case Branch(edges) =>
      val edge = find(edges)(e => commonPrefix(key, e.label).map((e, _)))
      edge match {
        case Some((e, (_, Some(prefix), None))) =>
          val newNode = e.node.put(stripPrefix(key, prefix), value)
          val newEdge = e.copy(node = newNode)
          Branch(replaceEdge[A, B](edges)(e -> newEdge))
        case Some((e, (Some(prefix), _, None))) =>
          val newLabel = stripPrefix(e.label, prefix)
          val newEdge = Edge(key, Branch(List(Edge(newLabel, e.node), Edge("", Leaf(value)))))
          Branch(replaceEdge[A, B](edges)(e -> newEdge))
        case Some((e, (None, None, Some(prefix)))) =>
          val le = Edge(stripPrefix(e.label, prefix), e.node)
          val re = Edge(stripPrefix(key, prefix), Leaf(value))
          val newEdge = Edge(prefix, Branch(List(le, re)))
          Branch(replaceEdge[A, B](edges)(e -> newEdge))
        case _ => Branch(Edge(key, Leaf(value)) :: edges)
      }
    case leaf@Leaf(_) if key.nonEmpty => Branch(List(Edge("", leaf), Edge(key, Leaf(value))))
    case Leaf(_) => Leaf(value)
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

  def startsWith(key: String): Boolean = getValue(key)(false)(_ => true)

  def get(key: String): List[A] = getValue(key)(List.empty[A]) {
    case Leaf(value) => List(value)
    case branch => branch.values
  }

  def containsLeaf(key: String) : Boolean = getValue(key)(false) {
    case Leaf(_) => true
    case _ => false
  }

  @tailrec
  private final def getValue[B](key: String)(default: B)(f : RadixTree[A] => B): B = (key.isEmpty, self) match {
    case (true, _) => f(self)
    case (false, Branch(xs)) => {
      val edge = find(xs)(e => if (e.label.nonEmpty) commonPrefix(key, e.label).map((e, _)) else None)
      edge match {
        case Some((e, (None, Some(prefix), None))) => e.node.getValue(stripPrefix(key, prefix))(default)(f)
        case Some((e, (Some(_), _, None))) => e.node.getValue("")(default)(f)
        case _ => default
      }
    }
    case _ => default
  }

}

object RadixTree {

  def empty[A]: RadixTree[A] = Branch(List.empty)

  def apply[A](xs: (String, A)*): RadixTree[A] = xs.foldLeft(empty[A]) {
    case (acc, (key, value)) => acc.put(key, value)
  }

  case class Edge[+A](label: String, node: RadixTree[A])

  case class Branch[A](edges: List[Edge[A]]) extends RadixTree[A]

  case class Leaf[A](value: A) extends RadixTree[A]

  private def find[A, B](xs: List[A])(f: A => Option[B]): Option[B] = {
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

  private def replaceEdge[A, B >: A](edges: List[Edge[A]])(replacement: (Edge[A], Edge[B])): List[Edge[B]] =
    replacement match {
      case (old, newOne) => newOne :: edges.filterNot(_ == old)
    }

  private def stripPrefix(x: String, prefix: String): String = x.substring(prefix.length)

  private[collection] def commonPrefix(x: String, y: String): Option[(Option[String], Option[String], Option[String])] = {

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