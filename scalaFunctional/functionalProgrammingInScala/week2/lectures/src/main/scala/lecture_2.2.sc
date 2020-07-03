def sum(f: Int => Int): (Int, Int) => Int = {
  def sumF(a: Int, b: Int): Int =
    if(a > b) 0
    else f(a) + sumF(a+1, b)

  sumF
}

def factorial(x: Int) : Int =
  if(x == 0) 1 else x * factorial(x - 1)

def sumInts = sum(x => x)
def sumCubes = sum(x => x * x * x)
def sumFactorials = sum(factorial)

sumCubes(1,10)

sum(x => x * x * x)(1,10)

def sum1(f: Int => Int)(a: Int, b: Int): Int =
  if(a > b) 0 else f(a) + sum(f)(a + 1, b)

def product(f: Int => Int)(a: Int, b: Int): Int = {
    if(a > b) 1
    else f(a) * product(f)(a+1, b)
}

product(x => x)(1, 10)

def factoria1(n: Int) = product(x => x)(1,n)

factoria1(3)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, ident: Int)(a: Int, b: Int) : Int = {
  if(a > b) ident
  else combine(f(a), mapReduce(f,combine,ident)(a + 1, b))
}

mapReduce(x => x, (lhs:Int, rhs: Int) => lhs * rhs,1)(1,3)

def productMapReduce(f: Int => Int)(a: Int, b: Int): Int =
  mapReduce(f, (lhs:Int, rhs: Int) => lhs * rhs,1)(a,b)

productMapReduce(x => x)(1, 10)