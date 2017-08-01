llvm IR compiler written in Scala

This is a very early work in proress.

The idea is to produce a full llc compiler written in scala the takes as input
LLVM assembly code, either in ascii or bitcode, and produced object code.

It is uses a hand coded parser for LLVM parser now but this will soon be
replaced by one from a parser generator, most likely we will be using
javacc.

The overriding idea is to produce initally a very compact and easily readible
compiler by using the power of scala.

Thought has been giving to how this will in the end be a compiler that also
can execute as fast as llc but this is not an overriding concern at this time.
