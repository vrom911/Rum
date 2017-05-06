package ru.ctddev.romashkina.compiler

fun interpretProgram(program: Program, globalVariables: HashMap<Expression.Var, Int> = hashMapOf()): HashMap<Expression.Var, Int> {
    var localVariables = HashMap<Expression.Var, Int>(globalVariables)
    var localFunctions = HashMap<String, Fun>()

    program.stmts.forEach { stmt ->
        when (stmt) {
            is Statement.Skip -> Unit

            is Statement.IfElse ->
                interpretProgram(if (stmt.cond.eval(localVariables) != 0) stmt.trueAct else stmt.falseAct, localVariables)

            is Statement.WhileDo -> {
                while (stmt.cond.eval(localVariables) != 0) interpretProgram(stmt.act, localVariables)
            }

            is Statement.RepeatUntil -> {
                do {
                    localVariables = interpretProgram(stmt.act, localVariables)
                } while (stmt.cond.eval(localVariables) == 0)
            }

            is Statement.For -> {
                localVariables = interpretProgram(stmt.init, localVariables)
                while (stmt.expr.eval(localVariables)!= 0) {
                    interpretProgram(stmt.body, localVariables)
                    interpretProgram(stmt.update, localVariables)
                }
            }

            is Statement.Assignment ->
                localVariables.put(stmt.left, stmt.right.eval(localVariables))
//                   globalVariables.computeIfPresent(stmt.left, {a, b -> stmt.right.eval(localVariables)})
            is Statement.WriteLn ->
                println(stmt.arg.eval(localVariables))

            is Statement.FunDeclaration -> {
                localFunctions.put(stmt.name, (stmt.func))
            }

        }
    }
    globalVariables.putAll(localVariables.filter { globalVariables.contains(it.key) })
//    println(localFunctions)
    return localVariables
}