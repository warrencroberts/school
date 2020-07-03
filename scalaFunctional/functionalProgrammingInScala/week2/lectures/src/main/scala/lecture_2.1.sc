def sumInts(a: Int, b: Int) : Int =
  if(a > b) 0 else a + sumInts(a+1, b)

def factorial(n: Int): Int =
  if(n == 0) 1 else n * factorial(n-1)

def id(x: Int): Int = x
def cube(x: Int): Int = x * x * x

def sumCubes(a: Int, b: Int) : Int =
  if(a > b) 0 else cube(a) + sumCubes(a + 1, b)

def sumFactorials(a: Int, b: Int) : Int =
  if(a > b) 0 else factorial(a) + sumFactorials(a + 1, b)

// Higher order function definition.
def sum(f: Int => Int, a: Int, b: Int): Int =
  if(a > b) 0
  else f(a) + sum(f, a+1, b)

def sumInts1(a: Int, b: Int) = sum(id, a, b)
def sumCubes1(a:Int, b:Int) = sum(cube, a, b)
def sumFactorials1(a: Int, b: Int) = sum(factorial, a, b)

// Simpler version using function literals
def sumInts2(a: Int, b: Int) = sum(x => x, a, b)
def sumCubes2(a: Int, b: Int) = sum(x => x*x*x, a, b)

def sum3(f: Int => Int)(a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a+1, acc + f(a))
  }
  loop(a, 0)
}

sum3(x => x*x)( 3, 5)

