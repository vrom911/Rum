package ru.ctddev.romashkina.compiler

import net.podkopaev.booleanComb.*
import java.lang.Math.pow

//////////////////////////////
// Data
/////////////////////////////

class Program(val stmts: List<Statement>) {
    override fun toString() = stmts.joinToString(separator = ";\n")
}

class Fun(val args: List<String>, val action: Program, val out: Expression)


sealed class Statement {
    class Assignment(val left: Expression.Var, val right: Expression): Statement() {
        override fun toString() = "$left := $right"
    }

    class WriteLn(val arg: Expression): Statement() {
        override fun toString() = "write($arg)"
    }

    class IfElse(val cond: Expression, val trueAct: Program, val falseAct: Program): Statement() {
        override fun toString() = "if ($cond)\nthen\n$trueAct\nelse\n$falseAct\nfi"
    }

    class RepeatUntil(val cond: Expression, val act: Program): Statement() {
        override fun toString() = "repeat\n${act}until\n$cond"
    }

    class  Skip: Statement() {
        override fun toString() = "skip"
    }

    class WhileDo(val cond: Expression, val act: Program): Statement() {
        override fun toString() = "while\n${cond}do\n${act}od"
    }

    class For(val init: Program, val expr: Expression, val update: Program, val body: Program): Statement() {
        override fun toString() = "for ($init, \t$expr, \t$update)\ndo\n${body}od"
    }

    class FunDeclaration(val name: String, val func: Fun): Statement() {
        override fun toString() = "fun $name(${func.args})\nbegin\n${func.action}return ${func.out}\nend"
    }

}

sealed class Expression {
    class Const(val value: Int): Expression() {
        override fun eval(env: Map<Var, Int>, funcs: Map<String, Fun>) = value
        override fun toString() = "$value"
    }

    class Var(val name: String): Expression() {
        override fun eval(env: Map<Var, Int>, funcs: Map<String, Fun>)
                = env[this] ?: throw IllegalArgumentException("Unknown variable $name")

        override fun toString() = name
    }

    class BinOper(val oper: String, val left: Expression, val right: Expression): Expression() {
        override fun eval(env: Map<Var, Int>, funcs: Map<String, Fun>): Int {
            val l = left.eval(env, funcs)
            val r = right.eval(env, funcs)
            return when (oper) {
                "+"  -> l + r
                "-"  -> l - r
                "*"  -> l * r
                "/"  -> l / r
                "%"  -> l % r
                "^"  -> pow(l.toDouble(), r.toDouble()).toInt()
                else -> throw IllegalArgumentException("Not supported operation")
            }
        }

        override fun toString() = "$left $oper $right"
    }

    class CompOper(val oper: String, val left: Expression, val right: Expression): Expression() {
        override fun eval(env: Map<Var, Int>, funcs: Map<String, Fun>): Int {
            val l = left.eval(env, funcs)
            val r = right.eval(env, funcs)
            return if (when (oper) {
                "==" -> l == r
                "!=" -> l != r
                "<"  -> l < r
                ">"  -> l > r
                "<=" -> l <= r
                ">=" -> l >= r
                else -> throw IllegalArgumentException("Not supported comparison")
            }) 1 else 0
        }

        override fun toString() = "$left $oper $right"
    }

    fun isTrue(value: Int): Boolean = value != 0

    class LogicOper(val oper: String, val left: Expression, val right: Expression): Expression() {
        override fun eval(env: Map<Var, Int>, funcs: Map<String, Fun>): Int {
            val l =  left.eval(env, funcs)
            val r = right.eval(env, funcs)
            return  if (when (oper) {
                "&&" -> isTrue(l) && isTrue(r)
                "||" -> isTrue(l) || isTrue(r)
                else -> throw IllegalArgumentException("Not supported operation $oper")
            }) 1 else 0
        }

        override fun toString() = "$left $oper $right"
    }

    class FunCall(val name: String, val args: List<Expression>): Expression() {
        override fun eval(env: Map<Var, Int>, funcs: Map<String, Fun>): Int {

//            funcs[name] ?: throw IllegalArgumentException("Function has not been declared yet")
            return funcs.get(name).out.eval(env, funcs)
        }
        override fun toString() = "$name($args)"
    }

    object ReadLn: Expression() {
        override fun eval(env: Map<Var, Int>, funcs: Map<String, Fun>): Int {
//            print("> ")
            return readLine()?.toInt() ?: throw IllegalArgumentException("Expected Int for read")
        }

        override fun toString() = "read()"
    }

    override fun hashCode(): Int = when (this) {
        is Const     -> value.hashCode()
        is Var       -> name.hashCode()
        is BinOper   -> oper.hashCode() + 31 * left.hashCode() + 31 * right.hashCode()
        is CompOper  -> oper.hashCode() + 31 * left.hashCode() + 31 * right.hashCode()
        is LogicOper -> oper.hashCode() + 31 * left.hashCode() + 31 * right.hashCode()
        is FunCall   -> name.hashCode() + 31 * args.hashCode()
           ReadLn    -> 1
    }

    override fun equals(other: Any?): Boolean {
        if (other == null || other !is Expression) return false
        when (other) {
            is Const     -> return this is Const    && value == other.value
            is Var       -> return this is Var      && name == other.name
            is BinOper   -> return this is BinOper  && oper == other.oper && left == other.left && right == other.right
            is CompOper  -> return this is CompOper && oper == other.oper && left == other.left && right == other.right
            is LogicOper -> return this is CompOper && oper == other.oper && left == other.left && right == other.right
            is FunCall   -> return this is FunCall  && name == other.name && args == other.args
            is ReadLn    -> return this is ReadLn
            else         -> throw IllegalArgumentException("Something with equals")
        }
    }

    abstract fun eval(env: Map<Var, Int>, funcs: Map<String, Fun>): Int
}

//////////////////////////////
// Expr Parsing
/////////////////////////////

fun litPars(lit: String, vararg lits: String) : Parser<String> {
    var p: Parser<String> = litp(lit)
    lits.forEach { p /= litp(it) }
    return sp(p)
}

val specialSymb: Parser<Char> = satp { arrayListOf('_', '-', '$').contains(it) }
val beginSpecialSymb: Parser<Char> = satp { arrayListOf('_','$').contains(it) }

val varName: Parser<String> =
        ((alpha / beginSpecialSymb) + many0(alphaOrDigit / specialSymb)) map {
            val sb = StringBuilder()
            sb.append(it.first)
            it.second.forEach { sb.append(it) }
            sb.toString()
        }

val varEP: Parser<Expression> = sp(varName) map { Expression.Var(it) }

// /  <|>
// map = <$> (fmap)
// + = <*> + (,) = liftA* (,)
// many0 = many
// many1 = some
// seqr = *>
// seql = <*
fun exprP(): Parser<Expression> = fix {
    val basicExprP: Parser<Expression> =
            (number map { Expression.Const(it) as Expression }) /
            (litPars("read()") map { Expression.ReadLn }) /
            varEP /
            paren(sp(it))

    val prior1 = rightAssocp(litPars("^"), basicExprP)
                { oper, l, r -> Expression.BinOper(oper, l, r) }
    val prior2 = leftAssocp(litPars("*", "/", "%"), prior1)
                { oper, l, r -> Expression.BinOper(oper, l, r) }
    val prior3 = leftAssocp(litPars("+", "-"), prior2)
                { oper, l, r -> Expression.BinOper(oper, l, r) }

    return@fix prior3
}

val compExprP: Parser<Expression>  = (exprP() + litPars("==", "!=", "<", ">", "<=", ">=") + exprP()) map {
    (a, b) -> Expression.CompOper(a.second, a.first, b) }

val exprPar = compExprP / exprP()

fun logicExpP(): Parser<Expression> = fix {
    val basic = exprPar / paren(sp(it))

    val prior1 = leftAssocp(litPars("&&"), basic)
                {oper, l, r -> Expression.LogicOper(oper, l, r)}
    val prior2 = leftAssocp(litPars("||"), prior1)
                {oper, l, r -> Expression.LogicOper(oper, l, r)}

    return@fix sp(prior2)
}
val funCallP: Parser<Expression> = (varName + paren(exprP() sepBy litPars(",") )) map {
    (name, args) -> Expression.FunCall(name, args)}

//////////////////////////////
// Stmt Parsing
/////////////////////////////

infix fun <A> Parser<A>.sepBy(sep: Parser<String>): Parser<List<A>> {
    return this + many0(sep seqr this) map { (fst, rest) -> listOf(fst) + rest}
}

fun stmtP(): Parser<Statement> = fix {

    val assignStmtP: Parser<Statement>  = ((varEP seql litPars(":=")) + logicExpP())  map {
                        (left, right) -> Statement.Assignment(left as Expression.Var, right) }

    val writeStmtP: Parser<Statement> = (litp("write") seqr logicExpP()) map {
                        arg -> Statement.WriteLn(arg) }

    val ifElseStmtp: Parser<Statement> =
            (litPars("if") seqr logicExpP()) +
            ((litPars("then") seqr (it sepBy litPars(";"))) seql litPars("fi")) map {
                (t, f) -> Statement.IfElse(t, Program(f), Program(listOf(Statement.Skip())))
            }

    val ifStmtP: Parser<Statement> = ifElseStmtp /
            ((litPars("if") seqr logicExpP()) +
                    (litPars("then") seqr (it sepBy litPars(";"))) +
                    (litPars("else") seqr (it sepBy litPars(";")) seql litPars("fi")) map {
                (t, f) -> Statement.IfElse(t.first, Program(t.second), Program(f))
            })

    val repeatStmtP: Parser<Statement> =
            ((litPars("repeat") seqr (it sepBy litPars(";"))) +
            (litPars("until") seqr logicExpP())) map {
                (act, cond) -> Statement.RepeatUntil(cond, Program(act))}

    val whileStmtP: Parser<Statement> =
            ((litPars("while") seqr logicExpP()) +
            (litPars("do") seqr (it sepBy litPars(";"))) seql litPars("od")) map {
                (cond, act) -> Statement.WhileDo(cond, Program(act)) }

    val forStmtP: Parser<Statement> =
            ((litPars("for") seqr (it sepBy litPars(";")) seql litPars(",") ) +
            (logicExpP() seql litPars(",")) +
            (it sepBy litPars(";")) +
            (litPars("do") seqr (it sepBy litPars(";")) seql litPars("od"))) map {
    (a, b) -> Statement.For(Program(a.first.first), a.first.second,Program(a.second), Program(b))
                    }

    val skipStmtP: Parser<Statement> = litPars("skip") map {Statement.Skip()}

    val funDeclP: Parser<Statement> = ((litPars("fun") seqr varName) +
            (paren(varName sepBy litPars(",")) seql litPars("begin")) +
            ((it sepBy litPars(";")) ) +
            (litPars("return") seqr logicExpP() seql ( litPars("end")))) map {
                (a, b) -> Statement.FunDeclaration(a.first.first, Fun(a.first.second, Program(a.second), b))}

    return@fix assignStmtP / writeStmtP / ifStmtP / repeatStmtP / whileStmtP / forStmtP / skipStmtP / funDeclP
}


//////////////////////////////
// Program Parsing
/////////////////////////////
val progP: Parser<List<Statement>> = stmtP() sepBy litPars(";")


