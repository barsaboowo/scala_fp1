{
  def product(f: Int => Int)(a:Int, b:Int) : Int = {
    if(a > b) 1
    else f(a) * product(f)(a+1,b)
  }

  def factorial(a:Int) : Int = product(x=>x)( 1, a)

  factorial(3)
  factorial(4)

  def op(f:(Int,Int)=>Int)(g:Int=>Int)(a:Int, b:Int, unit:Int) :Int = {
    if(a > b) unit
    else f(g(a), op(f)(g)(a+1, b, unit))
  }

  def fac2(a:Int) = op((x,y)=> x*y)(x=>x)(1, a, 1)

  fac2(4)
}