//
// Given a positive integer n, find all pairs of positive
// integers i and j, with 1 <= j < i < n such that i + j is prime
//

// First Solution
val n = 7

def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

((1 until n) flatMap (i =>
  (1 until i) map (j => (i,j)))) filter(pair => isPrime(pair._1 + pair._2))

// Second solution
for {
  i <- 1 until n
  j <- 1 until i
  if isPrime(i + j)
} yield (i, j)

// Scalar product
def scalarProduct1(xs: List[Double], ys: List[Double]): Double =
  (for {
    x <- xs
    y <- ys
  } yield x * y).sum

def scalarProduct2(xs: List[Double], ys: List[Double]): Double =
  (for ((x,y) <- xs zip ys) yield x * y).sum

