import java.io.File

fun main(args: Array<String>) {
//    val prog = listOf(
//            Statement.Assignment(
//                    left = Expression.Var("x"),
//                    right = Expression.ReadLn
//            ),
//            Statement.WriteLn(arg = Expression.Var("x"))
//    )
    val text = File("prog.vrom").readLines() // TODO: использовать readText
    println(text)
    val prog = parseProgram(text)
    println(prog)
    interpretProgram(prog)
}
