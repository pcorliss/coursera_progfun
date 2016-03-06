object session {

  def genCur(op: (Int, Int) => Int)(f: Int => Int)(a: Int, b: Int, c: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, op(acc, f(a)))
    }
    loop(a, c)
  }

  genCur((a, b) => (a + b))(x => x)(1, 4, 0)
  1 + 2 + 3 + 4
  genCur((a, b) => (a * b))(x => x)(1, 4, 1)
  1 * 2 * 3 * 4

  def sumGen(f: Int => Int)(a: Int, b: Int): Int = {
    genCur((a, b) => (a + b))(f)(a, b, 0)
  }
  def prodGen(f: Int => Int)(a: Int, b: Int): Int = {
    genCur((a, b) => (a * b))(f)(a, b, 1)
  }

  sumGen(x => x)(1, 4)
  1 + 2 + 3 + 4
  prodGen(x => x)(1, 4)
  1 * 2 * 3 * 4


  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc + f(a))
    }
    loop(a, 0)
  }

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc * f(a))
    }
    loop(a, 1)
  }

  def productFoo(f: Int => Int)(a: Int, b: Int): Int ={
    if (a > b) 1
    else f(a) * productFoo(f)(a+1, b)
  }

  def factorial(a: Int): Int = {
    product(x => x)(1, a)
  }

  product(x => x)(1, 4)
  productFoo(x => x)(1,4)
  1 * 2 * 3 * 4

  product(x => x)(1, 5)
  productFoo(x => x)(1,5)
  1 * 2 * 3 * 4 * 5

  factorial(5)


  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))


  def sumMap(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (a, b) => (a + b), 0)(a, b)
  def prodMap(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (a, b) => (a * b), 1)(a, b)

  sum(x => x * x)(1, 4)
  sumMap(x => x * x)(1, 4)
  product(x => x * x)(1, 4)
  prodMap(x => x * x)(1, 4)
}