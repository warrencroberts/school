def mapASegSeq[A,B](inp: Array[A], left: Int, right: Int, f: A => B, out: Array[B]) = {
  for(i <- left until right) {
    out(i) = f(inp(i))
  }
}

val in = Array(2,3,4,5,6)
val out = Array(0,0,0,0,0)
val f1 = (x:Int) => x*x
mapASegSeq(in, 1,3,f1,out)
out

import common._
def mapASegPar[A,B](inp: Array[A], left: Int, right: Int, f: A => B,
                    out: Array[B], threshold: Int) : Unit = {
  if(right-left < threshold)
    mapASegSeq(inp, left, right, f, out)
  else {
    val mid = left + (right -left)/2
    parallel(mapASegPar(inp, left, mid, f, out, threshold),
      mapASegPar(inp, mid, right, f, out, threshold))
  }
}

sealed abstract class ATree[A] {val size: Int}
case class ALeaf[A](a: Array[A]) extends ATree[A] {override val size = a.length}
case class ANode[A](l: ATree[A], r: ATree[A]) extends ATree[A] {
  override val size = l.size + r.size
}

def mapTreePar[A:Manifest, B:Manifest](t: ATree[A], f: A => B) : ATree[B] =
t match {
  case ALeaf(a) => {
    val len = a.length
    val b  = new Array[B](len)
    for(i <- 0 until len) {b(i)= f(a(i))}
    ALeaf(b)
  }
  case ANode(l, r) => {
    val (lb,rb) = parallel(mapTreePar(l, f), mapTreePar(r,f))
    ANode(lb, rb)
  }
}

sealed abstract class Tree[A]
case class Leaf[A](a: A) extends Tree[A]
case class Node[A](l: Tree[A], r: Tree[A]) extends Tree[A]

def reduce[A](t: Tree[A], f : (A,A) => A) : A =
t match {
  case Leaf(v) => v
  case Node(l,r) => f(reduce(l, f), reduce(r,f))
}

def tree = Node(Leaf(1), Node(Leaf(3), Leaf(8)))
def fMinus = (x: Int, y: Int) => x - y
reduce(tree, fMinus)

def reducePar[A](t: Tree[A], f : (A,A) => A) : A =
  t match {
    case Leaf(v) => v
    case Node(l,r) =>
      val (lv, rv) = parallel(reducePar(l, f), reducePar(r, f))
      f(lv, rv)
  }

def reduceSeg[A](inp: Array[A], left: Int, right: Int, f: (A,A) => A, threshhold: Int) : A = {
  if((right - left) < threshhold) {
    var res = inp(left)
    for(i <- left + 1 until right) res = f(res, inp(i))
    res
  } else {
    val mid = left + (right -left)/2
    val (a1, a2) = parallel(reduceSeg(inp, left, mid, f, threshhold),
      reduceSeg(inp, mid, right, f, threshhold))
    f(a1,a2)
  }
}
