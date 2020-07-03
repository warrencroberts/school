import common.parallel

def scanLeft[A](inp: Array[A], a0: A, f: (A,A) => A, out: Array[A]) : Unit = {
  out(0) = a0
  var a = a0
  for(i <- inp.indices) {
    a = f(a, inp(i))
    out(i+1) = a
  }
}

def reduceSegScan[A](inp: Array[A], right: Int, a0: A, f: (A,A) => A): A = {
  var a = a0
  for(i <- 0 until right) {
    a = f(a, inp(i))
  }
  a
}

def mapSegScanSeq[A,B](inp: Array[A], left: Int, right: Int, fi: (Int) => B, out: Array[B]) = {
  for(i <- left until right) {
    out(i) = fi(i)
  }
}

def mapSegScanPar[A,B](inp: Array[A], left: Int, right: Int, fi: (Int) => B, out: Array[B], threshold: Int): Unit = {
  if(right-left < threshold)
    mapSegScanSeq(inp, left, right, fi, out)
  else {
    val mid = left + (right -left)/2
    parallel(mapSegScanPar(inp, left, mid, fi, out, threshold),
      mapSegScanPar(inp, mid, right, fi, out, threshold))
  }
}

def scanLeft1[A](inp: Array[A], a0: A, f: (A,A) => A, out: Array[A], threshold: Int) = {
  val fi = (i: Int) => reduceSegScan(inp, i, a0, f)

  mapSegScanPar(inp, 0, inp.length, fi, out, threshold)
  val last = inp.length - 1
  out(last + 1) = f(out(last), inp(last))
}

val f2 = (x:Int,y:Int) => x+y
val outArr = Array(0,0,0,0)
scanLeft1(Array(1,3,8), 100, f2, outArr, 2)
outArr
