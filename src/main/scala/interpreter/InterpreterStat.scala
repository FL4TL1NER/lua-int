package interpreter

import cats.data.StateT

import nodes.Nodes.*

import Memory.*
import Stack.*
import Value.*

import Interpreter.*
import InterpreterExp.{executeExpr, executeExprlist}
import InterpreterExp.{valueToLong, valueToBool}
import scala.annotation.tailrec
import parser.Parser.parseVarlistAssignment
import parser.Parser.parseBlock
import parser.Parser.parseForInt
import parser.Parser.parseWhile

object InterpreterStat {
    def executeBlock(block: NodeBlock): IntState[Option[Seq[Value]]] = (
        for
            stats <- block.stats.foldLeft(emptyStat(Right[String,Option[Seq[Value]]](None)))((prev, stat) => StateT(input => {
                val prevRun = prev.run(input)
                for
                    prevRun <- prev.run(input)
                    statRun <- (
                        prevRun._2 match
                            case None => executeStat(stat).run(prevRun._1)
                            case Some(value) => Right(prevRun._1, Some(value))
                        )
                yield
                    statRun
            }))
            retStat <- (stats match
                case None => block.retstat match
                    case None => emptyStat(Right[String,Option[Seq[Value]]](None))
                    case Some(value) => executeRetstat(value).map(explist => Some(explist))
                case Some(value) => emptyStat(Right(Some(value))))
        yield
            retStat
    )

    def executeStat(stat: NodeStat): IntState[Option[Seq[Value]]] = (
        stat match
            case a: NodeDoEnd => executeDoEnd(a)
            case a: NodeForInt => executeForInt(a)
            case a: NodeIf => executeIf(a)
            case a: NodeStatEmpty => emptyStat(Right(None))
            case a: NodeLocalNamelist => executeLocalNamelist(a).map(_ => None)
            case a: NodeRepeat => executeRepeat(a)
            case a: NodeVarlistAssignment => executeVarlistAssignment(a).map(_ => None)
            case a: NodeWhile => executeWhile(a)
            case _ => executeError("WIP")
    )

    def executeRetstat(retstat: NodeRetstat): IntState[Seq[Value]] = (
        retstat.explist match
            case None => emptyStat(Right(Seq.empty))
            case Some(value) => executeExprlist(value)
    )

    def executeVarlistAssignment(assignment: NodeVarlistAssignment): IntState[Unit] = {
        def assignVar(var_ : NodeVar, value: Value): IntState[Unit] = {
            if var_.value.tail.length > 0 then
                executeError("WIP")
            else
                var_.value.start match
                    case EndNodeName(name) => assignGlobal(name, value)
                    case _ => executeError("unexpected var")
        }

        val varsLen = assignment.varlist.varlist.length
        for
            explist <- executeExprlist(assignment.explist)
            explistMod = (
                if explist.length > varsLen then
                    explist.take(varsLen)
                else if explist.length < varsLen then
                    explist++nilList(varsLen - explist.length)
                else
                    explist)
            varlist = assignment.varlist.varlist
            expvarlist = varlist zip explistMod
            _ <- expvarlist
            .map((v, e) => assignVar(v, e))
            .foldLeft(emptyStat(Right(())))((state, assvar) => {
                for
                    _ <- state
                    _ <- assvar
                yield
                    ()
            })
        yield
            ()
    }

    def executeDoEnd(doend: NodeDoEnd): IntState[Option[Seq[Value]]] = (
        for
            _ <- newScope
            ret <- executeBlock(doend.block)
            _ <- delScope
        yield
            ret
    )

    def executeForInt(for_ : NodeForInt): IntState[Option[Seq[Value]]] = {
        def executeBody(cur: Long): IntState[Option[Seq[Value]]] = (
            for
                _ <- newScope
                _ <- assignLocal(for_.name.name, Num(cur))
                ret <- executeBlock(for_.block)
                _ <- delScope
            yield
                ret
        )
        for
            init <- executeExpr(for_.exp1)
            initI <- emptyStat(valueToLong(init))
            limit <- executeExpr(for_.exp2)
            limitI <- emptyStat(valueToLong(limit))
            step <- executeExpr(for_.exp3)
            stepI <- emptyStat(valueToLong(step))
            ret <- (initI to limitI by stepI)
            .map(i => executeBody(i))
            .foldLeft(emptyStat(Right[String,Option[Seq[Value]]](None)))((prev, cur) => StateT(state => {
                for
                    a <- prev.run(state)
                    b <- (a._2 match
                        case None => cur.run(a._1)
                        case Some(_) => Right(a))
                yield
                    b
            }))
        yield
            ret
    }

    def executeIf(if_ : NodeIf): IntState[Option[Seq[Value]]] = {
        for
            exp <- executeExpr(if_.exp)
            expB = valueToBool(exp)
            ret <- (
                if expB then
                    for
                        _ <- newScope
                        ret <- executeBlock(if_.blockThen)
                        _ <- delScope
                    yield
                        ret
                else
                    if_.blockElse match
                        case None => emptyStat(Right(None))
                        case Some(value) => executeBlock(value))
        yield
            ret
    }

    def executeLocalNamelist(lNList: NodeLocalNamelist): IntState[Unit] = {
        def lVarAss(name: String, v: Value): IntState[Unit] = (
            assignLocal(name, v)
        )

        val varLen = lNList.attnamelist.namelist.length

        for
            expEval <- lNList.explist match {
                case None => emptyStat(Right[String, Option[Seq[Value]]](None))
                case Some(value) => executeExprlist(value).map(sV => Some(sV))}
            expList = expEval match
                case None => nilList(varLen)
                case Some(value) =>
                    if value.length > varLen then
                        value.take(varLen)
                    else if value.length < varLen then
                        value++nilList(varLen-value.length)
                    else
                        value
            nameExp = lNList.attnamelist.namelist.map(_._1.name) zip expList
            _ <- nameExp.map((n, v) => lVarAss(n, v)).fold(emptyStat(Right(())))((prev, cur) =>
                for
                    _ <- prev
                    _ <- cur
                yield
                    ())
        yield
            ()
    }

    def executeWhile(while_ : NodeWhile): IntState[Option[Seq[Value]]] = {
        for
            exp <- executeExpr(while_.exp)
            b = valueToBool(exp)
            _ <- newScope
            ret <- executeBlock(while_.block)
            _ <- delScope
            ret2 <- ret match {
                case None => if b then executeWhile(while_) else emptyStat(Right(None))
                case Some(a) => emptyStat(Right(ret))}
        yield
            ret2
    }

    def executeRepeat(repeat: NodeRepeat): IntState[Option[Seq[Value]]] = {
        for
            _ <- newScope
            ret <- executeBlock(repeat.block)
            exp <- executeExpr(repeat.exp)
            expB = valueToBool(exp)
            _ <- delScope
            ret2 <- ret match {
                case None => if expB then emptyStat(Right(None)) else executeRepeat(repeat)
                case Some(value) => emptyStat(Right(ret))}
        yield
            ret2
    }

    def nilList(n: Int): Seq[Nil] = {
        @tailrec
        def rec(n: Int, seq: Seq[Nil]): Seq[Nil] = (if n>1 then rec(n-1, seq:+Nil()) else seq)
        rec(n, Seq.empty)
    }
}

@main
def test() = {
    import Interpreter.*
    import InterpreterStat.*
    import parser.ParserExp.*
    import tokenizer.Tokenizer.*
    //print(parseExp.run(tokenize("""({name = 12, "das"})[2]""")))
    val test = "" +
      "a, b = 20, 45\n" +
      "while a ~= b do if a>b then a = a-b else b=b-a end end\n" +
      "return a"
    //print(parseWhile.run(tokenize("while a ~= b do if a>b then a = a-b else b=b-a end end")))
    parseBlock.run(tokenize(test)) match
        case Left(value) => print(value)
        case Right(tokens, exp) =>
            if tokens.isEmpty then
                print(
                    (for
                        exp2 <- executeBlock(exp)
                    yield
                        exp2).runA(emptyInt).toString())
            else
                print(tokens.map(_.s))
}
