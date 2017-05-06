package ru.ctddev.romashkina.compiler

class UnknownOperation(msg: String = "Not Supported Operation ", oper: String): Exception(msg + oper)