package interpreter

import cats.data.StateT

import nodes.Nodes.*

import Memory.*
import Stack.*
import Value.*

import scala.math.pow

import Interpreter.*

object InterpreterExp {
    def executeError[A](error: String): IntState[A] = StateT(input => {
        Left(error)
    })

    def executeExpr(expr: NodeExp): IntState[Value] = StateT(input => {
        (expr match
            case NodeBinOpExp(a, b, c) => executeBinExpr(NodeBinOpExp(a, b, c))
            case NodeUnOpExp(a, b) => executeUnExpr(NodeUnOpExp(a, b))
            case NodeValue(start, tail) => executeNodeValue(NodeValue(start, tail))
            case _ => executeSimpleExpr(expr)).run(input)
    })

    def executeBinExpr(expr: NodeBinOpExp): IntState[Value] = StateT(input => {
        (for
            exp1 <- executeExpr(expr.nodeExp)
            exp2 <- executeExpr(expr.nodeExp2)
        yield
            expr.binOp match
                case BinOp.Exponentiation => 
                    Num(pow(valueToDouble(exp1),valueToDouble(exp2)))
                case BinOp.Addition =>
                    Num(valueToDouble(exp1) + valueToDouble(exp2))
                case BinOp.Substaction =>
                    Num(valueToDouble(exp1) - valueToDouble(exp2))    
                case BinOp.Multiplication =>
                    Num(valueToDouble(exp1) * valueToDouble(exp2))
                case BinOp.Division =>
                    Num(valueToDouble(exp1) / valueToDouble(exp2))       
                case BinOp.FloorDivision =>
                    Num(valueToLong(exp1) / valueToLong(exp2))  

                case BinOp.Modulo =>
                    Num(valueToLong(exp1) % valueToLong(exp2))

                case BinOp.BitwiseAND =>
                    Num(valueToLong(exp1) & valueToLong(exp2))    
                case BinOp.BitwiseOR =>
                    Num(valueToLong(exp1) | valueToLong(exp2))      
                case BinOp.BitwiseXOR =>
                    Num(valueToLong(exp1) ^ valueToLong(exp2))     
                case BinOp.RightShift =>
                    Num(valueToLong(exp1) >> valueToLong(exp2))     
                case BinOp.LeftShift =>
                    Num(valueToLong(exp1) << valueToLong(exp2))      

                case BinOp.Equality =>
                    Bool(valueToBool(exp1) == valueToBool(exp2))        
                case BinOp.Inequality =>
                    Bool(valueToBool(exp1) != valueToBool(exp2))      
                case BinOp.LessThan =>
                    Bool(valueToBool(exp1) < valueToBool(exp2))        
                case BinOp.GreaterThan =>
                    Bool(valueToBool(exp1) > valueToBool(exp2))     
                case BinOp.LessOrEqual =>
                    Bool(valueToBool(exp1) <= valueToBool(exp2))     
                case BinOp.GreaterOrEqual =>
                    Bool(valueToBool(exp1) >= valueToBool(exp2))  

                case BinOp.And =>
                    if valueToBool(exp1) == false then
                        exp1
                    else
                        exp2    
                case BinOp.Or =>
                    if valueToBool(exp1) != false then
                        exp1
                    else
                        exp2             

                case BinOp.Concatenation =>
                    Str(valueToStr(exp1)+valueToStr(exp2))).run(input)
        
                

    })

    def executeUnExpr(expr: NodeUnOpExp): IntState[Value] = StateT(input => {
        (for
            exp <- executeExpr(expr.nodeExp)
        yield
            expr.unOp match
                case UnOp.Minus =>
                    Num(valueToDouble(exp)*(-1))
                case UnOp.BitwiseNOT =>
                    Num(~valueToLong(exp))
                case UnOp.Length =>
                    Num(valueToStr(exp).length().toLong)
                case UnOp.NOT =>
                    Bool(!valueToBool(exp))).run(input)
    })

    def executeSimpleExpr(expr: NodeExp): IntState[Value] = StateT(input => {
        val v = (expr match
            case EndNodeLiteralString(string) => Str(string)
            case EndNodeNumeral(a) => Num(a)
            case NodeBoolean(value) => Bool(value)
            case NodeNil() => Nil())
        Right(input, v)
    })

    def executeNodeValue(value: NodeValue): IntState[Value] = StateT((int) =>{
        (if value.tail.length > 0 then
            executeError("WIP")
        else
            value.start match
                case EndNodeName(name) => getData(name)
                case a: NodeExp => executeExpr(a)
                case _ => executeError("WIP")).run(int)
    })

    def executeTableConstructor(table: NodeTableconstructor): IntState[Value] = StateT((stack, memory) => {
        val expList = table.fields.collect(field => field match {case a: NodeExp => executeExpr(a)})
        .zipWithIndex
        .map((state, n) => (
            for
                exp <- state
            yield
                (exp, Num(n.toLong))
        ))
        val expExpList = table.fields.collect(
            field => field match {case NodeFieldExpExp(exp1, exp2) => (executeExpr(exp1), executeExpr(exp2))}
        ).map((state1, state2) => (
            for
                exp1 <- state1
                exp2 <- state2
            yield
                (exp1, exp2)
        ))
        val membersList = expList++expExpList
        
        for
            memoryAdr <- newEntry(Entry(0, Seq.empty, Table(Map.empty))).run(memory)
            a <- membersList.reduce().run((stack,memoryAdr._1))
        yield
        
        
    })
    //def executeBlock(block: NodeBlock): IntState[Seq[Value]] = ???

    def valueToDouble(v: Value): Double = {
        v match
            case Num(a) => a match {case a: Double => a; case a: Long => a.toDouble}
            case Str(s) => s.toDouble
    }
    def valueToLong(v: Value): Long = {
        v match
            case Num(a) => a match {case a: Double => a.toLong; case a: Long => a}
            case Str(s) => s.toLong
    }
    def valueToBool(v: Value): Boolean = {
        v match
            case Nil() => false
            case Bool(a) => a
            case _ => true
    }
    def valueToStr(v: Value): String = {
        v match
            case Num(a) => a.toString()
            case Bool(a) => a.toString()
            case Str(a) => a
            case Nil() => ""
    }
}

@main
def test() = {
    import Interpreter.*
    import InterpreterExp.*
    import parser.ParserExp.*
    import tokenizer.Tokenizer.*
    parseExp.run(tokenize("10 + c")) match
        case Left(value) => print(value)
        case Right(tokens, exp) => print(
            (for
                _ <- assignGlobal("a", Num(50L))
                _ <- assignLocal("b", Num(100L))
                _ <- newScope
                _ <- assignLocal("c", Num(75L))
                //_ <- delScope
                exp2 <- executeExpr(exp)
            yield
                exp2).run(emptyInt).toString())
}
