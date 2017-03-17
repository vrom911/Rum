import net.podkopaev.booleanComb.*
import java.lang.Math.pow

//////////////////////////////
// Data
/////////////////////////////

typealias Program = List<Statement>

sealed class Statement {
    class Assignment(val left: Expression.Var, val right: Expression): Statement()
    class WriteLn(val arg: Expression): Statement()
}

sealed class Expression {
    class Const(val value: Int): Expression() {
        override fun eval(env: Map<Var, Int>) = value
    }

    class Var(val name: String): Expression() {
        override fun eval(env: Map<Var, Int>)
                = env[this] ?: throw IllegalArgumentException("Unknown variable $name")
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
    }

    object ReadLn: Expression() {
        override fun eval(env: Map<Var, Int>): Int {
            print("> ")
            return readLine()?.toInt() ?: throw IllegalArgumentException("Expected Int for read")
        }
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
            ReadLn -> return this is ReadLn
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

val varP: Parser<Expression.Var> = sp(symbol) map { Expression.Var(it) }
val varEP: Parser<Expression> = sp(symbol) map { Expression.Var(it) }

fun exprP(): Parser<Expression> = fix {
    val exprPbase: Parser<Expression> =
            (number map { Expression.Const(it) as Expression }) /
            (litp("read()") map { Expression.ReadLn }) /
            varEP /
            paren(sp(it))

    val prior1 = rightAssocp(litPars("^"), exprPbase )
                { oper, l, r -> Expression.BinOper(oper, l, r) }
    val prior2 = leftAssocp(litPars("*", "/", "%"), prior1 )
                { oper, l, r -> Expression.BinOper(oper, l, r) }
    val prior3 = leftAssocp(litPars("+", "-"), prior2 )
                { oper, l, r -> Expression.BinOper(oper, l, r) }
    val prior4 = leftAssocp(litPars("==", "!=", "<", ">", "<=", ">="), prior3 ) // TODO: всего одни раз
                { oper, l, r -> Expression.CompOper(oper, l, r) }
    return@fix prior4
}

val stmtP: Parser<Statement> =
    ((varP seql litPars(":=")) + exprP() map { (left, right) -> Statement.Assignment(left, right) as Statement }) /
    (litp("write") seqr sp(paren(sp(exprP()))) map { arg -> Statement.WriteLn(arg) })

fun parseProgram(text: List<String>): Program {
    val program = mutableListOf<Statement>()
    for (line in text) {
        line.split(";").filter { it != "" }.forEach { // TODO: использовать parser combinators
            val statement = stmtP.get(it) ?: throw IllegalArgumentException("Invalid statement: $line")
            program.add(statement)
        }
    }
    return program
}

fun interpretProgram(program: Program) {
    val variables = hashMapOf<Expression.Var, Int>()

    for (statement in program) when (statement) {
        is Statement.Assignment -> variables.put(statement.left, statement.right.eval(variables))
        is Statement.WriteLn -> println(statement.arg.eval(variables))
    }
}