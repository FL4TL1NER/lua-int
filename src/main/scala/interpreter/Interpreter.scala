package interpreter

import cats.data.StateT
import scala.math.pow

import nodes.Nodes.*
import Memory.*
import Stack.*
import Value.*
import InterpreterExp.*

object Interpreter {
    type InterpreterState = (Stack, Memory)
    type IntOp[A] = Either[String, A]
    type IntState[A] = StateT[IntOp, InterpreterState, A]

    val emptyInt: InterpreterState = (emptyStack, emptyMemory)

    def getData(name: String): IntState[Value] = StateT((stack, memory) =>{
        getAddress(name).runA(stack) match
            case Left(value) => Right((stack, memory), Nil())
            case Right(address) =>
                address match
                    case None => Right((stack, memory), Nil())
                    case Some(address) =>
                        getEntry(address).runA(memory)
                        .map(entry => ((stack, memory), entry.value))
    })

    def createLocal(name: String): IntState[Unit] = StateT((stack, memory) => {
        addLocal(name, None).runS(stack)
        .flatMap(stack => Right((stack, memory), ()))
    })

    def createGlobal(name: String): IntState[Unit] = StateT((stack, memory) => {
        addLocal(name, None).runS(stack)
        .flatMap(stack => Right((stack, memory), ()))
    })

    def assignGlobal(name: String, value: Value): IntState[Unit] = StateT((stack, memory) => {
        getAddress(name).runA(stack).flatMap(adr => {
            adr match
                case None => 
                    newEntry(Entry(1, Seq.empty, value)).run(memory) match
                        case Left(value) => Left(value)
                        case Right(memory, address) =>
                            addGlobal(name, Some(address)).runS(stack)
                            .map(stack => ((stack, memory),()))
                case Some(adr) =>
                    changeEntry(adr, Entry(1, Seq.empty, value)).runS(memory)
                    .map(memory => ((stack, memory),()))
        })
    })

    def assignLocal(name: String, value: Value): IntState[Unit] = StateT((stack, memory) => {
        getAddress(name).runA(stack).flatMap(adr => {
            adr match
                case None => 
                    newEntry(Entry(1, Seq.empty, value)).run(memory) match
                        case Left(value) => Left(value)
                        case Right(memory, address) =>
                            addLocal(name, Some(address)).runS(stack)
                            .map(stack => ((stack, memory),()))
                case Some(adr) =>
                    changeEntry(adr, Entry(1, Seq.empty, value)).runS(memory)
                    .map(memory => ((stack, memory),()))
        })
    })

    val newScope: IntState[Unit] = StateT((stack, memory) => {
        push.runS(stack).map(stack => ((stack, memory), ()))
    })

    val delScope: IntState[Unit] = StateT((stack, memory) => {
        pop.run(stack).flatMap((stack, deleted) => {
            val a = deleted
            .map(_._2).collect(
                adr => adr match {case Some(adr) => decRefCount(adr)}
            ).toSeq
            if a.length > 0 then
                val del = a.reduce((a, b) => a.flatMap((Unit) => b))
                del.flatMap((Unit) => collectGarbage).runS(memory)
                .map(memory => ((stack, memory), ()))
            else
                Right((stack, memory), ())
        })
    })
}

@main
def test() = {
    import Interpreter.*
    import parser.ParserExp.*
    import tokenizer.Tokenizer.*
    parseExp.run(tokenize("a//2 == b//4")) match
        case Left(value) => print(value)
        case Right(tokens, exp) => print(
            (for
                _ <- assignGlobal("a", Num(50L))
                _ <- assignLocal("b", Num(100L))
                _ <- newScope
                _ <- assignLocal("c", Num(75L))
                _ <- delScope
                exp2 <- executeExpr(exp)
            yield
                exp2).run(emptyInt).toString())
}
