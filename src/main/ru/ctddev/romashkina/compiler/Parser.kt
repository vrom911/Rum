package ru.ctddev.romashkina.compiler

fun parseProgram(text: String): Program {
    val program = mutableListOf<Statement>()
    val statement = progP.get(text) ?: throw IllegalArgumentException("Invalid statement: $text")
    statement.forEach { program.add(it) }

    return Program(program)
}