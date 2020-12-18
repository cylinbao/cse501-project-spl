object Spl2 {
  import scala.collection.mutable.ArrayBuffer

  sealed trait Expr
  object Expr {
    case class Ident(name: String) extends Expr {
      override def toString(): String = name
    }

    case class Load(arr: Expr, index: List[Expr]) extends Expr
    case class Plus(a: Expr, b: Expr) extends Expr 
    case class Times(a: Expr, b: Expr) extends Expr
    case class Assign(a: Expr, b: Expr) extends Expr
    case class Range(a: Expr, b: Expr, step: Int) extends Expr
    case class For(ite: Expr, range: Range, body: Expr) extends Expr 
    case class Stmt(s: List[Expr]) extends Expr

    case class Array(name: String) extends Expr
    case class Matrix(name: String) extends Expr
    case class CsrMat(data: Expr, rowPtr: Expr, colInd: Expr) extends Expr
  }

  object CodeGenerator {
    import Expr._

    def genCode(node: Expr, indent: Int): String = {
      val space = s"%${indent}s".format(" ")

      node match {
        case Ident(name) => name
        case Plus(a, b) => {
          genCode(a, indent) + " + " + genCode(b, indent)
        }
        case Times(a, b) => {
          genCode(a, indent) + " * " + genCode(b, indent)
        }
        case Assign(a, b) => {
          space + genCode(a, indent) + " = " + genCode(b, indent)
        }
        case Load(arr, i) => {
          val v = genCode(arr, indent)
          val t = s"[${i.map(genCode(_, indent)).mkString(", ")}]"
          v + t
        }
        // case Range(st, end, step) =>
        case For(i, r, body) => {
          val vi = genCode(i, indent)
          val v0 = s"for(${vi} = ${genCode(r.a, indent)}; ${vi} < ${genCode(r.b, indent)}; ${vi} += ${r.step}) {\n"
          val v1 = genCode(body, indent+2)
          space + v0 + v1 + space + s"\n${space}}" // + space + "}"
        }
        case Stmt(l) => {
          val code = l.map(genCode(_, indent)).mkString("\n")
          code
        }
        case Array(s) => s
        case Matrix(s) => s
        case _ => {
          "not implemented"
        }
      }
    }
  }

  object Syntax {
    import Expr._

    def ident(s: String): Expr = Ident(s)
    def range(a: Expr, b: Expr, step: Int): Range = Range(a, b, step)

    def array(s: String) = Array(s)
    def matrix(s: String) = Matrix(s)
    def csrMat(s: String) = {
      CsrMat(array(s"$s.data"), array(s"$s.rowPtr"), array(s"$s.colInd"))
    }

    // def update(a: Expr, b: Expr): Expr = Assign(a, b)

    implicit class IntOperations(n: Int) {
      def int = Ident(n.toString())
    }

    implicit class SyntaxOperations(a: Expr) {
      def plus(b: Expr) = Plus(a, b)
      def assign(b: Expr) = Assign(a, b)
      def times(b: Expr) = Times(a, b)

      def sum(arr: Expr, i: Expr, range: Range) = {
        val acc = Ident("acc")
        val init = Assign(acc, 0.int)
        val body = Assign(acc, Plus(acc, Load(arr, List(i))))
        val l = For(i, range, body)
        val out = Assign(a, acc)
        Stmt(List(init, l, out))
      }
    }

    implicit class CsrOperations(a: Expr) {
      def sumCsrRow(csr: CsrMat, b: Matrix, row: Expr, col: Expr) = {
        val k = Ident("k")
        val row_str = row.toString()
        val end = Ident(row_str+"+1")
        val r = range(Load(csr.rowPtr, List(row)), Load(csr.rowPtr, List(end)), 1)

        val acc = Ident("acc")
        val init = Assign(acc, 0.int)
        val b_i = Load(csr.colInd, List(k))

        val b_v = Load(b, List(b_i, col))
        val a_v = Load(csr.data, List(k))
        val mul = Times(a_v, b_v)

        val p = Plus(acc, mul)
        val body = Assign(acc, p)

        val l = For(k, r, body)
        val out = Assign(a, acc)
        Stmt(List(init, l, out))
      }

      def replaceIdent(in_s: String, out_s: String): Expr = {
        a match {
          case Ident(s) => {
            if (s == in_s) {
              Ident(out_s)
            } else {
              a
            }
          }
          case Plus(a, b) => {
            Plus(a.replaceIdent(in_s, out_s),
                 b.replaceIdent(in_s, out_s))            
          }
          case Times(a, b) => {
            Times(a.replaceIdent(in_s, out_s),
                  b.replaceIdent(in_s, out_s))
          }
          case Assign(a, b) => {
            Assign(a.replaceIdent(in_s, out_s),
                   b.replaceIdent(in_s, out_s))
          }
          case Load(arr, l) => {
            val n_l = l.map(_.replaceIdent(in_s, out_s))
            Load(arr, n_l)
          }
          case _ => a
        }
      }

      def csrRowTile(factor: Int): Expr = {
        a match {
          case For(i, r, body) => {
            val new_s = r.step * factor
            // val new_b0 = body.replaceIdent(i.toString(), s"${i.toString()}*$factor + t")
            val new_b0 = body.replaceIdent("k", s"k*$factor + t")

            val new_b1 = For(Ident("t"), range(0.int, factor.int, 1), new_b0)
            val f = For(i, range(r.a, r.b, new_s), new_b1)
            f
          }
          case Stmt(l) => {
            val s = l.map(_.csrRowTile(factor))
            Stmt(s)
          }
          case _ => a
        }
      }
    }

    implicit class ArrayOperations(a: Array) {
      def apply(i: Expr) = Load(a, List(i))
    }
  
    implicit class MatrixOperations(a: Matrix) {
      def apply(i: Expr, j: Expr) = Load(a, List(i, j))
    }

    case class kernel(name: String) {
      def apply(root: Expr) = {root}
    }
  
    case class loop(i: Expr, r: Range) {
      def apply(body: Expr) = For(i, r, body)
    }
  }
}

object TestSpl2 extends App {
  import Spl2.Syntax._
  import Spl2.CodeGenerator._

  val k = kernel("kernel") {
    var i = ident("i")
    var j = ident("j")
    
    var A = csrMat("A")
    var B = matrix("B")
    var C = matrix("C")
    
    loop(i, range(1.int, 128.int, 1)) {
      loop(j, range(1.int, 128.int, 1)) {
        C(i, j).sumCsrRow(A, B, i, j).csrRowTile(32)
      }
    }
  } 
  println(k)
  
  val func_str = "void CsrSpmmKernel(CsrMat A, Matrix B, Matrix C) {"

  // val code = genCode(k.root, 2)
  println("***** Generated Code *****")
  println()
  println(func_str)
  println(genCode(k, 2))
  println("}")
}