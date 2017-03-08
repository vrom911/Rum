import net.podkopaev.whileParser.Expr

fun main(args: Array<String>) {
    println(Expr.Binop("+", Expr.Con(2), Expr.Con(3))?.calc(emptyMap()))
}
