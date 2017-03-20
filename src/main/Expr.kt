import java.io.File

fun main(args: Array<String>) {
//    val prog = listOf(
//            Statement.IfElse(
//                    cond = Expression.CompOper("==", Expression.Const(1), Expression.Const(1)),
//                    trueAct = Statement.WriteLn(arg = Expression.Const(0)),
//                    falseAct = Statement.WriteLn(arg = Expression.Const(100))
//            ),
//            Statement.WriteLn(arg = Expression.Const(999))
//    )
    val text = File("prog.vrom").readText()
//    println(text)
    val prog = parseProgram(text)
    println(prog)
    interpretProgram(prog)
}
