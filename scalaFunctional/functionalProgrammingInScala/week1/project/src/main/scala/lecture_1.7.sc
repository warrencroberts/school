def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a%b)

gcd(14, 21)

def factorial(n: Int): Int =
  if(n == 0) 1 else n * factorial(n-1)

factorial(10)

def factorialTail(n: Int) = {
  def factorial(n: Int, acc: Int): Int = {
//    println(s"(n = $n, acc = $acc")
    if(n == 0)
      acc
    else {
      factorial(n - 1, acc * n)
    }
  }

  factorial(n, 1)
}

factorialTail(0)
factorialTail(1)
factorialTail(2)
factorialTail(10)
