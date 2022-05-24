package hwk10

abstract class Exp {
  // return the string representation of this expression 
  override def toString: String = this match {
    // TODO
	case Plus(x, y) => "(" + x.toString + " + " + y.toString + ")"
	case Minus(x, y) => "(" + x.toString + " - " + y.toString + ")"
	case Times(x, y) => "(" + x.toString + " * " + y.toString + ")"
	case Div(x, y) => "(" + x.toString + " / " + y.toString + ")"
	case Var(x) => x
	case Let(x,e1,e2) => "let val " + x + " = " + e1.toString + " in " + e2.toString + " end"
	case App(e1,e2) => "(" + e1.toString + " " + e2.toString + ")"
	case Fn(x,e) => "(fn " + x + " => " + e.toString + ")"
  }
  
  // lookup 'y' in 'ctx'
  def lookup (ctx: List[(String, Value)], y: String): Value = ctx match {
    // TODO
    case List(x, v)::ctx => 
      if(x == y) v
      else lookup (ctx, y)
  }

  def eval: Value = this eval List()
  
  // evaluate 'this' expression with the context 'ctx'
  def eval (ctx: List[(String, Value)]): Value = this match {
    // TODO
    case e of
      (Int x) => CVal x
      (Plus(e1,e2)) => 
        (case (eval e1 ctx, eval e2 ctx) of
            (CVal x, CVal y) => CVal(x + y)
            (Error m, _) => Error m
            (_, Error m) => Error m
            (v, CVal _) => Error("Plus error: " + v.Value + " is not a number")
            (_, v) => Error("Plus error: " + v.Value + " is not a number")
        )
      (Minus(e1,e2)) => 
        (case (eval e1 ctx, evla e2 ctx) of
            (CVal x, CVal y) => CVal (x - y)
            (Error m, _) => Error m
            (_, Error m) = Error m
            (v, CVal _) = Error("Minus error: " + v.Value + " is not a number")
            (_, v) => Error("Minus error: " + v.Value + " is not a number")
        )
      (Times(e1,e2)) => 
        (case (eval e1 ctx, eval e2 ctx) of
            (CVal x, CVal y) => CVal(x * y)
            (Error m, _) => Error m
            (_, Error m) => Error m 
            (v, CVal _) => Error("Times error: " + v.Value + " is not a number")
            (_, v) => Error("Times error: " + v.Value + " is not a number")
        )
      (Div(e1,e2)) =>
        (case (eval e1 ctx, eval e2 ctx) of
            (_, CVal 0) => Error ("Division by zero error: " + e.toString())
            (Error m, _) => Error m
            (_, Error m) => Error m
            (Cval x, Cval y) => Cval(x div y)
            (v, CVal _) => Error("Division error: " + v.Value + " is not a number")
            (_, v) => Error("Division error: " + v.Value + " is not a number")
        )
      (Var x) => lookup ctx x
      (Let(x,e1,e2)) => 
        (case eval e1ctx of 
            Error m => Error m
            v => eval e2 ((x, v)::ctx))
      (App(e1, e2)) => (case (eval e1 ctx, eval e2 ctx) of
          (FnVal(x, e, ctx1), v) => eval e ((x, v) :: ctx1)
          (Error m, _) => Error m
          (_, Error m) => Error m
          (v, _) => Error("Application error: " + v.Value + " is not a function"))
      (Fn(x, e)) => FnVal(x, e, ctx)
      
  }
}

case class Num(x: Int) extends Exp
case class Plus(e1: Exp, e2: Exp) extends Exp
case class Minus(e1: Exp, e2: Exp) extends Exp
case class Times(e1: Exp, e2: Exp) extends Exp
case class Div(e1: Exp, e2: Exp) extends Exp
case class Var(x: String) extends Exp
case class Let(x: String, e1: Exp, e2: Exp) extends Exp
case class App(e1: Exp, e2: Exp) extends Exp
case class Fn(x: String, e: Exp) extends Exp


abstract class Value {
  // return the string representation of this value
  override def toString = this match {
    // TODO
    case FnVal(x, e, _) => "(fn " + x + " => " + e.toString + ")"
    case Error(m) => m
  }
}
case class CVal(x: Int) extends Value
case class FnVal(x: String, e: Exp, ctx: List[(String, Value)]) extends Value
case class Error(m: String) extends Value

object hwk10 {
  def main(args: Array[String]) {
    val t1 = Let("y", Num(10), 
                  Let("f", Fn("x", Plus(Var("x"), Var("y"))), 
                           Let("y", Num(20),
                                    App(Var("f"), Num(5)))))
                                    
    val t2 = Let("y", Num(10), 
                  Let("f", Fn("x", Plus(Var("x"), Var("z"))), 
                           Let("y", Num(10),
                                    App(Var("f"), Num(5)))))
    
    val t3 = Div(Num(10), Num(0))
    
    val t4 = Plus(Num(10), Minus (Num(10), Fn("x", Num(5))))
    
    val t5 = App(Num(10), Num(10))
    
    val t6 = Let("f", Num(10), App(Var("f"), Num(20)))
    
    val t7 = Let("x", Plus(Num(10), Fn("x", Var("x"))), Plus(Num(0), Num(20)))
    
    println(t1.eval)
    println(t2.eval)
    println(t3.eval)
    println(t4.eval)
    println(t5.eval)
    println(t6.eval)
    println(t7.eval)
  }
}

