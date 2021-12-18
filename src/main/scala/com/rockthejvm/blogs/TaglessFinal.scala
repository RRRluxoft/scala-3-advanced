package com.rockthejvm.blogs

object TaglessFinal {

  // expression problem
  object ExpressionProblem {
    trait Expr
    case class B(boolean: Boolean)          extends Expr
    case class Or(left: Expr, right: Expr)  extends Expr
    case class And(left: Expr, right: Expr) extends Expr
    case class Not(expr: Expr)              extends Expr

    val aGiantBoolean: Expr = Or(And(B(true), B(false)), B(false))

    def eval(expr: Expr): Boolean = expr match {
      case B(b)             => b
      case Or(l, r)         => eval(l) || eval(r)
      case And(left, right) => eval(left) && eval(right)
      case Not(expr)        => !eval(expr)
    }

    // include ints:
    case class I(int: Int)           extends Expr
    case class Sum(a: Expr, b: Expr) extends Expr

    def eval_v2(expr: Expr): Boolean | Int = expr match {
      case B(b)             => b
      case Or(l, r)         => eval(l).asInstanceOf[Boolean] || eval(r).asInstanceOf[Boolean]
      case And(left, right) => eval(left).asInstanceOf[Boolean] && eval(right).asInstanceOf[Boolean]
      case Not(expr)        => !eval(expr).asInstanceOf[Boolean]
    }

  }

  object Solution1 {
    trait Expr(val tag: String)
    case class B(boolean: Boolean)          extends Expr("bool")
    case class Or(left: Expr, right: Expr)  extends Expr("bool") {
      assert(left.tag == "bool" || right.tag == "bool")
    }
    case class And(left: Expr, right: Expr) extends Expr("bool")
    case class Not(expr: Expr)              extends Expr("bool")
    case class I(int: Int)                  extends Expr("int")
    case class Sum(a: Expr, b: Expr)        extends Expr("int")

    def eval(expr: Expr): Boolean | Int = expr match {
      case B(b)            => b
      case Or(left, right) =>
        eval(left).asInstanceOf[Boolean] || eval(right).asInstanceOf[Boolean]
    }
  }

  object TagLessInitial {

    import com.rockthejvm.blogs.TaglessFinal.ExpressionProblem.Expr

    trait Expr[A]
    case class B(boolean: Boolean)                            extends Expr[Boolean]
    case class Or(left: Expr[Boolean], right: Expr[Boolean])  extends Expr[Boolean]
    case class And(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean]
    case class Not(expr: Expr[Boolean])                       extends Expr[Boolean]
    case class I(int: Int)                                    extends Expr[Int]
    case class Sum(a: Expr[Int], b: Expr[Int])                extends Expr[Int]

    def eval[A](expr: Expr[A]): A = expr match {
      case I(int)                                        => int
      case B(bool)                                       => bool
      case Or(left: Expr[Boolean], right: Expr[Boolean]) => eval[Boolean](left) || eval(right)
      case Sum(a, b)                                     => eval(a) + eval(b)
      //etc
    }
  }

  object TaglessFinal {
    trait Expr[A] {
      val value: A // the final value we care about
    }
    def b(boolean: Boolean): Expr[Boolean] = new Expr[Boolean] {
      val value = boolean
    }
    def i(int: Int): Expr[Int] = new Expr[Int] {
      val value = int
    }
    def or(left: Expr[Boolean], right: Expr[Boolean]): Expr[Boolean] = new Expr[Boolean] {
      val value = left.value || right.value
    }
    def and(left: Expr[Boolean], right: Expr[Boolean]): Expr[Boolean] = new Expr[Boolean] {
      val value = left.value && right.value
    }
    def sum(left: Expr[Int], right: Expr[Int]): Expr[Int] = new Expr[Int] {
      val value = left.value + right.value
    }

    def eval[A](expr: Expr[A]): A = expr.value
  }

  object TaglessFinal_V2 {
    trait Algebra[E[_]] {
      def b(boolean: Boolean): E[Boolean]
      def i(int: Int): E[Int]
      def or(left: E[Boolean], right: E[Boolean]): E[Boolean]
      def and(left: E[Boolean], right: E[Boolean]): E[Boolean]
      def sum(a: E[Int], b: E[Int]): E[Int]
    }

    case class SimpleExpr[A](value: A)
    given simpleAlgebra: Algebra[SimpleExpr] with {
      override def b(boolean: Boolean): SimpleExpr[Boolean] = SimpleExpr(boolean)
      override def i(int: Int): SimpleExpr[Int] = SimpleExpr(int)
      override def or(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]): SimpleExpr[Boolean] =
        SimpleExpr(left.value || right.value)
      override def and(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]): SimpleExpr[Boolean] =
        SimpleExpr(left.value && right.value)
      override def sum(a: SimpleExpr[Int], b: SimpleExpr[Int]): SimpleExpr[Int] =
        SimpleExpr(a.value + b.value)
    }

    def program1[E[_]](using alg: Algebra[E]): E[Boolean] = {
      import alg.*
      and(b(true), or(b(false), b(true)))
    }
    def program2[E[_]](using alg: Algebra[E]): E[Int] = {
      import alg.*
      sum(i(43), sum(i(-1), i(100)))
    }
  }

  def demoTagless(): Unit = {
    import TagLessInitial.*
    println(eval(Or(B(true), And(B(true), B(false)))))
    println(eval(Sum(I(43), Sum(I(-1), I(100)))))
  }

  def demoTaglessFinal(): Unit = {
    import TaglessFinal.*
    println(eval(and(b(true), or(b(false), b(true)))))
    println(eval(sum(i(43), sum(i(-1), i(100)))))
  }

  def demoFinalTagless_V2(): Unit = {
    import TaglessFinal_V2.*
    println(program1[SimpleExpr])
    println(program2.value)
  }

  @main def tagless: Unit = {
    demoTagless()
    demoTaglessFinal()
    demoFinalTagless_V2()
  }
}
