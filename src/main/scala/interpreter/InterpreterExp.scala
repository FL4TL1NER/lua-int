package interpreter

import cats.data.StateT

import nodes.Nodes.*

import Memory.*
import Stack.*
import Value.*

import scala.math.pow

import Interpreter.*

object InterpreterExp {
    def valueToKey(v: Value): Either[String, Num | Str | Bool] = {
        v match
            case a: Bool => Right(a)
            case a: Num => Right(a)
            case s: Str => Right(s)
            case _ => Left("not a key")
    }

    def executeExpr(expr: NodeExp): IntState[Value] = StateT(input => {
        (expr match
            case a: NodeBinOpExp => executeBinExpr(a)
            case a: NodeUnOpExp => executeUnExpr(a)
            case a: NodeSimpleexp => executeSimpleExpr(a)).run(input)
    })

    def executeBinExpr(expr: NodeBinOpExp): IntState[Value] = StateT(input => {
        def binExpState(binOp: BinOp, val1: Value, val2: Value): IntState[Value] = StateT(input => {
            val a = expr.binOp match
                case BinOp.Exponentiation =>
                    for
                        a <- valueToDouble(val1)
                        b <- valueToDouble(val2)
                    yield
                        Num(pow(a, b))
                case BinOp.Addition =>
                    for
                        a <- valueToDouble(val1)
                        b <- valueToDouble(val2)
                    yield
                        Num(a + b)
                case BinOp.Substaction =>
                    for
                        a <- valueToDouble(val1)
                        b <- valueToDouble(val2)
                    yield
                        Num(a - b)   
                case BinOp.Multiplication =>
                    for
                        a <- valueToDouble(val1)
                        b <- valueToDouble(val2)
                    yield
                        Num(a * b)
                case BinOp.Division =>
                    for
                        a <- valueToDouble(val1)
                        b <- valueToDouble(val2)
                    yield
                        Num(a / b)       
                case BinOp.FloorDivision =>
                    for
                        a <- valueToLong(val1)
                        b <- valueToLong(val2)
                    yield
                        Num(a / b)  
                case BinOp.Modulo =>
                    for
                        a <- valueToLong(val1)
                        b <- valueToLong(val2)
                    yield
                        Num(a % b)
                case BinOp.BitwiseAND =>
                    for
                        a <- valueToLong(val1)
                        b <- valueToLong(val2)
                    yield
                        Num(a & b)    
                case BinOp.BitwiseOR =>
                    for
                        a <- valueToLong(val1)
                        b <- valueToLong(val2)
                    yield
                        Num(a | b)      
                case BinOp.BitwiseXOR =>
                    for
                        a <- valueToLong(val1)
                        b <- valueToLong(val2)
                    yield
                        Num(a ^ b)    
                case BinOp.RightShift =>
                    for
                        a <- valueToLong(val1)
                        b <- valueToLong(val2)
                    yield
                        Num(a >> b)     
                case BinOp.LeftShift =>
                    for
                        a <- valueToLong(val1)
                        b <- valueToLong(val2)
                    yield
                        Num(a << b)
                case BinOp.And =>
                    Right(if valueToBool(val1) == false then
                        val1
                    else
                        val2 )   
                case BinOp.Or =>
                    Right(if valueToBool(val1) != false then
                        val1
                    else
                        val2)
                case BinOp.Equality =>
                    Right(valEquality(val1, val2))
                case BinOp.Inequality =>
                    Right({
                        val a = valEquality(val1, val2)
                        Bool(!a.a)
                    })
                case BinOp.LessThan =>
                    for
                        a <- valueToDouble(val1)
                        b <- valueToDouble(val2)
                    yield
                        Bool(a < b)
                case BinOp.GreaterThan =>
                    for
                        a <- valueToDouble(val1)
                        b <- valueToDouble(val2)
                    yield
                        Bool(a > b)
                case BinOp.LessOrEqual =>
                    for
                        a <- valueToDouble(val1)
                        b <- valueToDouble(val2)
                    yield
                        Bool(a <= b)
                case BinOp.GreaterOrEqual =>
                    for
                        a <- valueToDouble(val1)
                        b <- valueToDouble(val2)
                    yield
                        Bool(a >= b)
                case BinOp.Concatenation =>
                    for
                        a <- valueToStr(val1)
                        b <- valueToStr(val2)
                    yield
                        Str(a+b)
            a.map(a => (input, a))
        })
        (for
            exp1 <- executeExpr(expr.nodeExp)
            exp2 <- executeExpr(expr.nodeExp2)
            a <- binExpState(expr.binOp, exp1, exp2)
        yield
            a).run(input)
    })

    def executeUnExpr(expr: NodeUnOpExp): IntState[Value] = StateT(input => {
        def unExpState(unOp: UnOp, val_ : Value): IntState[Value] = StateT(input => {
            val a = unOp match
                case UnOp.Minus =>
                    valueToDouble(val_).map(a => Num(a* (-1)))
                case UnOp.BitwiseNOT => 
                    valueToLong(val_).map(a => Num(~a))
                case UnOp.Length =>
                    valLength(val_).map(a => Num(a))
                case UnOp.NOT => 
                    Right(Bool(valueToBool(val_)))
            a.map(a => (input, a))
        })
        (for
            exp <- executeExpr(expr.nodeExp)
            val_ <- unExpState(expr.unOp, exp)
        yield
            val_).run(input)     
    })

    def executeSimpleExpr(expr: NodeSimpleexp): IntState[Value] = StateT(input => {
        (expr match
            case EndNodeLiteralString(string) => emptyStat(Right(Str(string)))
            case EndNodeNumeral(a) => emptyStat(Right(Num(a)))
            case NodeBoolean(value) => emptyStat(Right(Bool(value)))
            case NodeNil() => emptyStat(Right(Nil()))
            case a: NodeValue => executeNodeValue(a)
            case a: NodeFunctiondef => emptyStat(Left("WIP"))
            case a: NodeTableconstructor => executeTableConstructor(a)
            case a: NodeVararg => emptyStat(Left("WIP"))).run(input)
    })

    def executeNodeValue(value: NodeValue): IntState[Value] = StateT((int) => {
        def executeIndex(v: Value, index: NodeVar_index): IntState[Value] = (
            for
                ind <- executeExpr(index.nodeExp)
                key <- emptyStat(valueToKey(ind))
                tableAdr <- emptyStat(v match {
                    case Table(a) => Right(a.get(key))
                    case a => Left(s"${a.toString()} not indexable")})
                ret <- tableAdr match {
                    case None => emptyStat(Right(Nil()))
                    case Some(value) => memState(getEntry(value)).map(entry => entry.value)}
            yield
                ret
        )

        def executeHead(head: EndNodeName | NodeExp): IntState[Value] = (
            head match
                case EndNodeName(name) => getData(name)
                case a: NodeExp => executeExpr(a)
        )

        (if value.tail.length > 0 then
            value.tail.foldLeft(executeHead(value.start))((head, tail) => 
                for
                    v <- head
                    res <- tail match {
                        case a: NodeVar_index => executeIndex(v, a)
                        case a: NodeFunctioncall_args => executeError("WIP")}
                yield
                    res)
        else
            executeHead(value.start)).run(int)
    })

    def executeTableConstructor(table: NodeTableconstructor): IntState[Table] = StateT((stack, memory) => {
        val expList: Seq[IntState[(Num | Str | Bool, Value)]] = table.fields.collect(field => field match {case a: NodeExp => executeExpr(a)})
        .zipWithIndex
        .map((state, n) => (
            for
                exp <- state
            yield
                (Num(n.toLong+1), exp)
        ))
        val expExpList: Seq[IntState[(Num | Str | Bool, Value)]] = table.fields.collect(
            field => field match {case NodeFieldExpExp(exp1, exp2) => (executeExpr(exp1), executeExpr(exp2))}
        ).map((state1, state2) => (
            for
                exp1 <- state1
                exp2 <- state2
                key <- StateT((input: InterpreterState) => {valueToKey(exp1).map(exp => (input, exp))})
            yield
                (key, exp2)
        ))

        val b = expList++expExpList

        val stateAddress = (
            for
                newTable <- b.foldLeft(emptyStat(Right(Map.empty[Num | Str | Bool, Address])))((exp1, exp2) =>
                    for
                        ad <- exp1
                        keyValue <- exp2
                        valAdress <- memState(newEntry(Entry(0, Seq.empty, keyValue._2)))
                    yield
                        ad + (keyValue._1 -> valAdress)
                )
            yield
                newTable
        )       
        
        stateAddress.run((stack, memory)).map((state, address) => (state, Table(address)))
    })
    //def executeBlock(block: NodeBlock): IntState[Seq[Value]] = ???

    def executeExprlist(explist: NodeExplist): IntState[Seq[Value]] = {
        explist.explist.foldLeft(emptyStat(Right(Seq.empty[Value])))((list, exp) => {
            for
                list_ <- list
                res <- executeExpr(exp)
            yield
                (list_ :+ res)
        })
    }

    def valLength(v: Value): Either[String, Long] = {
        v match
            case Table(a) => Right(a.size)
            case Str(s) => Right(s.length())
            case a => Left(s"${a.toString()} does not have length")
    }

    def valEquality(v1: Value, v2: Value): Bool = {
        (v1, v2) match
            case (Num(a), Num(b)) => Bool(a==b)
            case (Bool(a), Bool(b)) => Bool(a==b)
            case (Str(a), Str(b)) => Bool(a==b)
            case (_, _) => Bool(false)
    }

    def valueToNum(v: Value): Either[String, Double | Long] = {
        v match
            case Num(a) => Right(a)
            case a => Left(s"${a.toString()}")
    }

    def valueToDouble(v: Value): Either[String, Double] = {
        v match
            case Num(a) => Right(a match {case a: Double => a; case a: Long => a.toDouble})
            case Str(s) => Right(s.toDouble)
            case a => Left(s"attempt to convert ${a} to Double")
    }
    def valueToLong(v: Value): Either[String, Long] = {
        v match
            case Num(a) => Right(a match {case a: Double => a.toLong; case a: Long => a})
            case Str(s) => Right(s.toLong)
            case a => Left(s"attempt to convert ${a} to Long")
    }
    def valueToBool(v: Value): Boolean = {
        v match
            case Nil() => false
            case Bool(a) => a
            case _ => true
    }
    def valueToStr(v: Value): Either[String, String] = {
        v match
            case Num(a) => Right(a.toString())
            case Bool(a) => Right(a.toString())
            case Str(a) => Right(a)
            case a => Left(s"attempt to convert ${a.toString()} to string")
    }
}

// @main
// def test() = {
//     import Interpreter.*
//     import InterpreterExp.*
//     import parser.ParserExp.*
//     import tokenizer.Tokenizer.*
//     //print(parseExp.run(tokenize("""({name = 12, "das"})[2]""")))
//     parseExplist.run(tokenize("""1, nil""")) match
//         case Left(value) => print(value)
//         case Right(tokens, exp) =>
//             if tokens.isEmpty then
//                 print(
//                     (for
//                         _ <- assignGlobal("a", Num(50L))
//                         _ <- assignLocal("b", Num(100L))
//                         _ <- newScope
//                         _ <- assignLocal("c", Num(75L))
//                         //_ <- delScope
//                         exp2 <- executeExprlist(exp)
//                     yield
//                         exp2).run(emptyInt).toString())
//             else
//                 print(tokens.map(_.s))
// }
