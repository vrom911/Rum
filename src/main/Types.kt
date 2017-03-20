import net.podkopaev.booleanComb.*
import java.lang.Math.pow

//////////////////////////////
// Data
/////////////////////////////

class Program(var stmts: List<Statement>) {
    override fun toString(): String {
        val progText = StringBuilder()
        stmts.forEach { progText.append(it.toString() + ";\n") }
        return progText.toString().removeSuffix(";\n")
    }
}

sealed class Statement {
    class Assignment(val left: Expression.Var, val right: Expression): Statement() {
        override fun toString() = "$left := $right"
    }

    class WriteLn(val arg: Expression): Statement() {
        override fun toString() = "write($arg)"
    }

    class IfElse(val cond: Expression, val trueAct: Program, val falseAct: Program): Statement() {
        override fun toString() = "if ($cond)\nthen $trueAct\nelse $falseAct\nfi"
    }
}

sealed class Expression {
    class Const(val value: Int): Expression() {
        override fun eval(env: Map<Var, Int>) = value
        override fun toString() = "$value"
    }

    class Var(val name: String): Expression() {
        override fun eval(env: Map<Var, Int>)
                = env[this] ?: throw IllegalArgumentException("Unknown variable $name")

        override fun toString() = name
    }

    class BinOper(val oper: String, val left: Expression, val right: Expression): Expression() {
        override fun eval(env: Map<Var, Int>): Int {
            val l = left.eval(env)
            val r = right.eval(env)
            return when (oper) {
                "+" -> l + r
                "-" -> l - r
                "*" -> l * r
                "/" -> l / r
                "%" -> l % r
                "^" -> pow(l.toDouble(), r.toDouble()).toInt()
                else -> throw IllegalArgumentException("Not supported operation")
            }
        }

        override fun toString() = "$left $oper $right"
    }

    class CompOper(val oper: String, val left: Expression, val right: Expression): Expression() {
        override fun eval(env: Map<Var, Int>): Int {
            val l = left.eval(env)
            val r = right.eval(env)
            return if (when (oper) {
                "==" -> l == r
                "!=" -> l != r
                "<" -> l < r
                ">" -> l > r
                "<=" -> l <= r
                ">=" -> l >= r
                else -> throw IllegalArgumentException("Not supported comparison")
            }) 1 else 0
        }

        override fun toString() = "$left $oper $right"
    }

    object ReadLn: Expression() {
        override fun eval(env: Map<Var, Int>): Int {
            print("> ")
            return readLine()?.toInt() ?: throw IllegalArgumentException("Expected Int for read")
        }

        override fun toString() = "read()"
    }

    override fun hashCode(): Int = when (this) {
        is Const -> value.hashCode()
        is Var -> name.hashCode()
        is BinOper -> oper.hashCode() + 31 * left.hashCode() + 8 * right.hashCode()
        is CompOper -> oper.hashCode() + 31 * left.hashCode() + 8 * right.hashCode()
        ReadLn -> 1
    }

    override fun equals(other: Any?): Boolean {
        if (other == null || other !is Expression) return false
        when (other) {
            is Const -> return this is Const && value == other.value
            is Var -> return this is Var && name == other.name
            is BinOper -> return this is BinOper && oper == other.oper && left == other.left && right == other.right
            is CompOper -> return this is CompOper && oper == other.oper && left == other.left && right == other.right
            is ReadLn -> return this is ReadLn
            else -> throw IllegalArgumentException("Something with equals")
        }
    }

    abstract fun eval(env: Map<Expression.Var, Int>): Int
}

//////////////////////////////
// Functions
/////////////////////////////

fun litPars(lit: String, vararg lits: String) : Parser<String> {
    var p: Parser<String> = litp(lit)
    lits.forEach { p /= litp(it) }
    return sp(p)
}

val varEP: Parser<Expression> = sp(symbol) map { Expression.Var(it) }

fun exprP(): Parser<Expression> = fix {
    val basicExprP: Parser<Expression> =
            (number map { Expression.Const(it) as Expression }) /
            (litPars("read()") map { Expression.ReadLn }) /
            varEP /
            paren(sp(it))

    val prior1 = rightAssocp(litPars("^"), basicExprP )
                { oper, l, r -> Expression.BinOper(oper, l, r) }
    val prior2 = leftAssocp(litPars("*", "/", "%"), prior1 )
                { oper, l, r -> Expression.BinOper(oper, l, r) }
    val prior3 = leftAssocp(litPars("+", "-"), prior2 )
                { oper, l, r -> Expression.BinOper(oper, l, r) }

    return@fix prior3
}

val exprPar = leftAssocp(litPars("==", "!=", "<", ">", "<=", ">="), exprP() )
                { oper, l, r -> Expression.CompOper(oper, l, r) }

infix fun <A> Parser<A>.sepBy(sep: Parser<String>): Parser<List<A>> {
    return this + many0(sep seqr this) map { (fst, rest) -> listOf(fst) + rest}
}

fun stmtP(): Parser<Statement> = fix {
    val basicStmtP: Parser<Statement> =
            ((varEP seql litPars(":=")) + (exprPar)  map { (left, right) -> Statement.Assignment(left as Expression.Var, right) as Statement }) /
                    (litp("write") seqr sp(paren(sp(exprPar)))  map { arg -> Statement.WriteLn(arg) })

    val ifStmtP =
            (litPars("if") seqr exprP()) +
            (litPars("then") seqr (it sepBy litPars(";"))) +
            (litPars("else") seqr (it sepBy litPars(";")) seql litPars("fi")) map { (t, f) -> Statement.IfElse(t.first, Program(t.second), Program(f)) as Statement }

    return@fix basicStmtP / ifStmtP
}

val stPar: Parser<List<Statement>> = stmtP() sepBy litPars(";")

fun parseProgram(text: String): Program {
    val program = mutableListOf<Statement>()
    val statement = stPar.get(text) ?: throw IllegalArgumentException("Invalid statement: $text")
    statement.forEach { program.add(it) }

    return Program(program)
}

fun interpretProgram(program: Program, globalVariables: HashMap<Expression.Var, Int> = hashMapOf<Expression.Var, Int>()) {
    val localVariables = HashMap<Expression.Var, Int>(globalVariables)

    program.stmts.forEach {
        val stmt: Statement? = it
        when (stmt) {
            is Statement.IfElse ->
                interpretProgram(if (stmt.cond.eval(localVariables) == 1) stmt.trueAct else stmt.falseAct, localVariables)
            is Statement.Assignment ->
                localVariables.put(stmt.left, stmt.right.eval(localVariables))
            is Statement.WriteLn ->
                println(">> ${stmt.arg.eval(localVariables)}")
        }

    }
    globalVariables.putAll(localVariables.filter { globalVariables.contains(it.key) })

}