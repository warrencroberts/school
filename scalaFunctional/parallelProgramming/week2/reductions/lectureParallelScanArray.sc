import common.parallel

sealed abstract class TreeResA[A] {val res: A}
case class LeafA[A](from: Int, to: Int,
                    override val res: A) extends TreeResA[A]
case class NodeA[A](l: TreeResA[A], override val res: A, r: TreeResA[A]) extends TreeResA[A]

def upsweepA[A](inp: Array[A], from: Int, to: Int, f: (A,A) => A, threshold: Int) : TreeResA[A] = {
  if(to - from < threshold) {
    LeafA(from, to, reduceSegA1(inp, from + 1, to, inp(from), f))
  } else {
    val mid = from + (to -from)/2
    val (tL, tR) = parallel(upsweepA(inp, from, mid, f, threshold),
      upsweepA(inp, mid, to, f, threshold))
    NodeA(tL, f(tL.res, tR.res), tR)
  }
}

def reduceSegA1[A](inp: Array[A], left: Int, right: Int, a0: A, f: (A,A) => A): A = {
  var a = a0
  for(i <- left until right) a = f(a, inp(i))
  a
}

def downsweepA[A](inp: Array[A], a0: A, f: (A,A) => A, t: TreeResA[A], out: Array[A]): Unit =
  t match {
    case LeafA(from, to, res) => scanLeftSegA(inp, from, to, a0, f, out)
    case NodeA(l, _, r) => parallel(
      downsweepA(inp, a0, f, l, out),
      downsweepA(inp, f(a0, l.res), f, r, out))
  }

def scanLeftSegA[A](inp: Array[A], left: Int, right: Int, a0: A,
                    f: (A,A) => A, out: Array[A]) = {
  if(left < right) {
    var a = a0
    for(i <- left until right) {
      a = f(a, inp(i))
      out(i + 1) = a
    }
  }
}

def scanLeft[A](inp: Array[A], a0: A, f: (A,A) => A, out: Array[A], threshold: Int) = {
  val t = upsweepA(inp, 0, inp.length, f, threshold )
  downsweepA(inp, a0, f, t, out)
  out(0) = a0
}