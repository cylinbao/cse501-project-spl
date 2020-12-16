import scala.collection._

object IR {
  sealed trait Expr
  case class Num(num: Int) extends Expr {
    // override def toString(): String = num.toString() 
  }
  case class Node(s: String) extends Expr {
    override def toString(): String = s 
  }
  case class IteVar(s: String) extends Expr {
    override def toString(): String = s 
  }
  case class Buffer(s: String, i: Expr) extends Expr
  case class Plus(a: Expr, b: Expr) extends Expr 
  case class Times(a: Expr, b: Expr) extends Expr
  case class Range(a: Expr, b: Expr, s: Int) extends Expr
  case class Eq(a: Expr, b: Expr) extends Expr
  case class ForLoop(i: IteVar, r: Range, body: Expr) extends Expr 
  case class Stmt(s: List[Expr]) extends Expr

  trait CodeGen extends Expr
  case class CodeGenBase(a: String) extends CodeGen {
    override def toString() = a
  }

  case class Matrix(s: String, i: Expr, j: Expr) extends Expr
  case class CsrMatrix(data: Expr, rowPtr: Expr, colInd: Expr)
}

import IR._

sealed trait SplOperations {
  def num(a: Int): Num = Num(a)
  def node(v: String): Node = Node(v)
  def iteVar(v: String): IteVar = IteVar(v)
  def buffer(v: String, i: Expr): Expr = Buffer(v, i)
  
  def plus(a: Expr, b: Expr): Expr = Plus(a, b)
  def times(a: Expr, b: Expr): Expr = Times(a, b)
  def equal(a: Expr, b: Expr): Expr = Eq(a, b)
  def range(a: Expr, b: Expr, s: Int): Range = Range(a, b, s)
  def loop(i: IteVar, r: Range, b: Expr): Expr = ForLoop(i, r, b)
  def stmt(s: List[Expr]): Expr = Stmt(s)

  def matrix(s: String, i: Expr, j: Expr): Matrix = Matrix(s, i, j)
  def csrMatrix(data: Expr, rowPtr: Expr, colInd: Expr): CsrMatrix = CsrMatrix(data, rowPtr, colInd)
}

object CodeGenerator {
  import IR._

  def genCode(node: Expr, indent: Int): String = {
    val space = s"%${indent}s".format(" ")
    //println("space length")
    //println(space.length)
    node match {
      case Num(num) => num.toString() 
      case Node(v) => v
      case IteVar(v: String) => v
      case Buffer(s, i) => s + s"[${i.toString()}]"
      case Eq(a, b) => {
        val v = space + genCode(a, indent) + " = " + genCode(b, indent)
        v
      }
      case Plus(a, b) => genCode(a, indent) + " + " + genCode(b, indent)
      case Times(a: Expr, b: Expr) => genCode(a, indent) + " * " + genCode(b, indent)
      case Matrix(s: String, i: Expr, j: Expr) => s + s"[${genCode(i, indent)}, ${genCode(j, indent)}]"
      case ForLoop(i: IteVar, r: Range, b: Expr) => {
        val v0 = s"for(int $i = ${genCode(r.a, indent)}; $i < ${genCode(r.b, indent)}; ${i} += ${r.s}){\n"
        val v1 = genCode(b, indent+2)
        space + v0 + v1 + space + "\n" + space + "}"
      }
      case Stmt(l) => {
        val code = l.map(genCode(_, indent)).mkString("\n")
        code
      }
      case _ => {
        "not implemented"
      }
    }
  }
}

object SplOperationsSyntax {
  val o = new SplOperations {}

  implicit class SyntaxOperations(a: Expr) {
    def equal(b: Expr): Expr = {
      o.equal(a, b)
    }
    def times(b: Expr): Expr = {
      o.times(a, b)
    }
    def replaceIteVar(os: String, ns: String): Expr = {
      val out: Expr = a match {
        case Num(num) => Num(num)
        case Node(v) => Node(v)
        case IteVar(v: String) => {
          val out = v match {
            case os => IteVar(ns)
            case _ => IteVar(v)
          }
          out
        }
        case Buffer(s, i) => {
          val ii = i.replaceIteVar(os, ns)
          Buffer(s, ii)
        }
        case Eq(a, b) => {
          Eq(a.replaceIteVar(os, ns), b.replaceIteVar(os, ns))
        }
        case Plus(a, b) => {
          Plus(a.replaceIteVar(os, ns),
               b.replaceIteVar(os, ns))
        }
        case Times(a: Expr, b: Expr) => {
          Times(a.replaceIteVar(os, ns),
                b.replaceIteVar(os, ns))
        }
        case Matrix(s: String, i: Expr, j: Expr) => {
          Matrix(s, i.replaceIteVar(os, ns),
                 j.replaceIteVar(os, ns))
        }
        case _ => {
          a
        }
      }
      out
    }
    def csrRowTile(t: Int): Expr = {
      val out: Expr = a match {
        case Stmt(s) => {
          val ss = s.map(_.csrRowTile(t))
          Stmt(ss)
        }
        case ForLoop(i, r, body) => {
          val new_s = r.s * t
          val new_b = body.replaceIteVar(i.s, s"${i.s}*$t + x")
          val new_b1 = ForLoop(IteVar("x"), Range(Num(0), Num(t), 1), new_b)
          val fl = ForLoop(i, Range(r.a, r.b, new_s), new_b1)
          fl
        }
        case Num(num) => Num(num)
        case Node(v) => Node(v)
        case IteVar(v: String) => IteVar(v)
        case Buffer(s, i) => Buffer(s, i)
        case Eq(a, b) => {
          Eq(a.csrRowTile(t),
             b.csrRowTile(t))
        }
        case Plus(a, b) => {
          Plus(a.csrRowTile(t),
               b.csrRowTile(t))
        }
        case Times(a: Expr, b: Expr) => {
          Times(a.csrRowTile(t),
                b.csrRowTile(t))
        }
        case Matrix(s: String, i: Expr, j: Expr) => {
          Matrix(s, i.csrRowTile(t),
                 j.csrRowTile(t))
        }
        case _ => {
          a
        }
      }
      out
    }
  }

  def node(a: String) = o.node(a)
  def num(n: Int) = o.num(n)
  def iteVar(a: String) = o.iteVar(a)
  def buffer(v: String, i: Expr) = o.buffer(v, i)
  def range(a: Expr, b: Expr, s: Int) = o.range(a, b, s)
  def loop(i: IteVar, r: Range, b: Expr) = o.loop(i, r, b)
  def matrix(s: String, i: Expr, j: Expr) = o.matrix(s, i, j)
  def csrMatrix(data: Expr, rowPtr: Expr, colInd: Expr) = o.csrMatrix(data, rowPtr, colInd)
  
  def reduce(in: Expr, out: Expr, r: Range, i: IteVar): Expr = {
    val tmp = o.node("tmp")
    val a = o.equal(tmp, num(0))
    val body = o.equal(tmp, o.plus(tmp, in))
    val l = o.loop(i, r, body)
    val c = o.equal(out, tmp)
    o.stmt(List(a, l, c))
  } 
  
  def csrRowIte(A: CsrMatrix, B: Matrix, C: Matrix): Expr = {
    val end = buffer("A.rowPtr", iteVar("i+1"))
    val r_csr = range(A.rowPtr, end, 1)

    val k = iteVar("k")
    val compute = (A.data).times(B)
    val foo = reduce(compute, C, r_csr, k)
    foo
  }
}

class Program() { 
  import SplOperationsSyntax._
  /*
  def test0() = {
    val a = node("a")
    val e = a.equal(node("b"))
    e
  }

  def test1() = {
    val i = iteVar("i")
    val j = iteVar("j")
    val r = range(num(0), num(4), 1)
    val arr = buffer("arr", i)
    val brr = buffer("brr", j)
    // val foo = arr.equal(num(0))
    val foo = reduce(brr, arr, r, j)
    val foo2 = loop(i, r, foo)
    foo2
  }
  */
  def test1() = {
    val i = iteVar("i")
    val i_1 = iteVar("i+1")
    val j = iteVar("j")
    val k = iteVar("k")
    val r = range(num(0), num(64), 1)

    val a_data = buffer("A.data", k)
    val a_row = buffer("A.rowPtr", i)
    val a_row_1 = buffer("A.rowPtr", i_1)
    val a_col = buffer("A.colInd", k)

    val csr = csrMatrix(a_data, a_row, a_col)

    val r_csr = range(a_row, a_row_1, 1)

    // val A = csrMatrix(a_data, a_row, a_col)
    val B = matrix("B", a_col, j)
    val C = matrix("C", i, j)

    val compute = a_data.times(B)

    val foo = reduce(compute, C, r_csr, k)

    val foo1 = loop(j, r, foo)
    val foo2 = loop(i, r, foo1)
    foo2
  }

  def test2() = {
    val i = iteVar("i")
    val i_1 = iteVar("i+1")
    val j = iteVar("j")
    val k = iteVar("k")
    val r = range(num(0), num(64), 1)

    val a_data = buffer("A.data", k)
    val a_row = buffer("A.rowPtr", i)
    val a_row_1 = buffer("A.rowPtr", i_1)
    val a_col = buffer("A.colInd", k)

    val csr = csrMatrix(a_data, a_row, a_col)

    val B = matrix("B", a_col, j)
    val C = matrix("C", i, j)

    val ite = csrRowIte(csr, B, C)

    val foo1 = loop(j, r, ite)
    val foo2 = loop(i, r, foo1)
    foo2
  }

  def test3() = {
    val i = iteVar("i")
    // val i_1 = iteVar("i+1")
    val j = iteVar("j")
    val k = iteVar("k")
    val r = range(num(0), num(64), 1)

    val a_data = buffer("A.data", k)
    val a_row = buffer("A.rowPtr", i)
    // val a_row_1 = buffer("A.rowPtr", i_1)
    val a_col = buffer("A.colInd", k)

    val csr = csrMatrix(a_data, a_row, a_col)

    val B = matrix("B", a_col, j)
    val C = matrix("C", i, j)

    val ite = csrRowIte(csr, B, C).csrRowTile(32)

    val foo1 = loop(j, r, ite)
    val foo2 = loop(i, r, foo1)
    foo2
  }
}

object TestSpl extends App {
  import CodeGenerator._
  
  //implicit val o = IROperations.ops
  // implicit val o = CodeGenOperations.ops
  val func_str = "void CSRSpMM(CstMat A, Mat b, Mat C) {"
  
  val code = new Program()

  println(code.test1)
  println()
  println("--------------------")
  println("Test 1: Regular CSR SpMM")
  println("********************")
  println("** GENERATED CODE **")
  println("********************")
  println()
  println(func_str)
  println()
  println(genCode(code.test1, 2))
  println()
  println("}")  

  println()
  println("--------------------")
  println("Test 2: csrRowIte")
  println("********************")
  println("** GENERATED CODE **")
  println("********************")
  println()
  println(func_str)
  println()
  println(genCode(code.test2, 2))
  println()
  println("}")

  println()
  println("--------------------")
  println("Test 3: csrRowIte + Tiling")
  println("********************")
  println("** GENERATED CODE **")
  println("********************")
  println()
  println(func_str)
  println()
  println(genCode(code.test3, 2))
  println()
  println("}")
}