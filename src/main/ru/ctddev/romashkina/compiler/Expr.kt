package ru.ctddev.romashkina.compiler

import java.io.File

fun main(args: Array<String>) {
    if (args[0] != "-i") throw IllegalArgumentException("Illegal command")
    val progFileName = args[1]
    if (!progFileName.endsWith(".expr")) throw IllegalArgumentException("Wrong file extension $progFileName")
    val text = File(progFileName).readText()
    val prog = parseProgram(text)
//    println(prog)
    interpretProgram(prog)
}
