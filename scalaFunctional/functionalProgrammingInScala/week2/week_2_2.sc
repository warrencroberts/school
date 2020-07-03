object exercise {
	def product(f: Int => Int)(a:Int, b:Int) : Int = {
			if (a > b)
				1
			else
				f(a) * product(f)(a+1, b)
	}

	def fact(x:Int) : Int = product(x => x) (1, x)

	def mapReduce(f: Int =>Int, combine: (Int, Int) => Int, unitValue: Int )
					 (a:Int, b:Int): Int = {
		if(a > b)
			unitValue
		else
			combine(f(a), mapReduce(f, combine, unitValue)(a+1 , b))
	}

	def product1(f: Int => Int)(a : Int, b: Int) : Int =
		mapReduce(f, (c1, c2) => c1 * c2, 1)(a, b)


	product(x => x*x)(3,4)
	product1(x => x*x)(3,4)
	fact(5)
	mapReduce(x => x, (c1, c2) => c1 * c2 , 1)(1,3)
}