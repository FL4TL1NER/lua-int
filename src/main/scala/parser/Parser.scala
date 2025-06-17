package parser

import scala.annotation.tailrec

import cats.data.StateT

import tokenizer.Tokenizer.{Token, TokenType}
import nodes.Nodes.*
import ParserUtils.*
import parser.ParserExp.{parseExp, parseExplist, parseFuncbody, parseVar, parseFunctioncall}

type parseRes[A] = Either[String, A]
type ParseState[A] = StateT[parseRes, Seq[Token], A]

object Parser {
    val parseRetstat: StateT[parseRes, Seq[Token], NodeRetstat] = (
        for
            _ <- parseWord(_.s == "return", "no return")
            explist <- combOpt(parseExplist)
        yield
            NodeRetstat(explist)
    )
    val parseStat = StateT[parseRes, Seq[Token], NodeStat](input =>{
        takeNthToken(input, 1).flatMap((_, token) => {
            (token match
                case Token(";", _) => parseEmpty
                case Token("::", _) => parseLabel
                case Token("break", _) => parseBreak
                case Token("goto", _) => parseGoto
                case Token("do", _) => parseDoEnd
                case Token("while", _) => parseWhile
                case Token("repeat", _) => parseRepeat
                case Token("if", _) => parseIf
                case Token("for", _) => parseFor
                case Token("function", _) => parseFunction
                case Token("local", _) => parseLocal
                case _ => parseAssFuncCall).run(input)
        })
    })

    val parseBlock: StateT[parseRes, Seq[Token], NodeBlock] = (
        for
            stats <- combList(parseStat)
            retstat <- combOpt(parseRetstat)
        yield
            NodeBlock(stats, retstat)
    )
    val parseChunk: StateT[parseRes, Seq[Token], NodeChunk] = (
        for
            block <- parseBlock
        yield
            NodeChunk(block)
    )

    val parseName = parseWord(_.t == TokenType.name, "not a name").map(t => EndNodeName(t.s))

    val parseNamelist: StateT[parseRes, Seq[Token], NodeNamelist] = (
        for
            name <- parseName
            namelist <- combList(
                for
                    _ <- parseWord(_.s == ",", "")
                    name <- parseName
                yield
                    name
            )
        yield
            NodeNamelist(Seq(name)++namelist)
    )
    val parseVarlist: StateT[parseRes, Seq[Token], NodeVarlist] = (
        for
            var_ <- parseVar
            varlist <- combList(
                for
                    _ <- parseWord(_.s == ",", "")
                    var_ <- parseVar
                yield
                    var_
            )
        yield
            NodeVarlist(Seq(var_)++varlist)
    )

    val parseEmpty: ParseState[NodeStatEmpty] = parseWord(_.s == ";", "not a ;").map(_ => NodeStatEmpty())
    val parseLabel: ParseState[NodeLabel] = (
        for
            _ <- parseWord(_.s == "::", "expected :: 1")
            name <- parseName
            _ <- parseWord(_.s == "::", "expected :: 2")
        yield
            NodeLabel(name)
    )
    val parseBreak: ParseState[NodeBreak] = parseWord(_.s == "break", "not a break").map(_ => NodeBreak())
    val parseGoto: ParseState[NodeGoto] = (
        for
            _ <- parseWord(_.s == "goto", "not a goto")
            name <- parseName
        yield
            NodeGoto(name)
    ) 
    val parseDoEnd: StateT[parseRes, Seq[Token], NodeDoEnd] = (
        for
            _ <- parseWord(_.s == "do", "not a do")
            block <- parseBlock
            _ <- parseWord(_.s == "end", "not a end")
        yield
            NodeDoEnd(block)
    ) 
    val parseWhile: ParseState[NodeWhile] = (
        for
            _ <- parseWord(_.s == "while", "not a while")
            exp <- parseExp
            _ <- parseWord(_.s == "do", "not a do")
            block <- parseBlock
            _ <- parseWord(_.s == "end", "not a end")
        yield
            NodeWhile(exp, block)
    ) 
    val parseRepeat: ParseState[NodeRepeat] = (
        for
            _ <- parseWord(_.s == "repeat", "not a repeat")
            block <- parseBlock
            _ <- parseWord(_.s == "until", "not a until")
            exp <- parseExp
        yield
            NodeRepeat(block, exp)
    ) 
    val parseIf: ParseState[NodeIf] = (
        for
            _ <- parseWord(_.s == "if", "not a if")
            exp <- parseExp
            _ <- parseWord(_.s == "then", "not a then")
            block <- parseBlock
            elseIfList <- combList(
                for
                    _ <- parseWord(_.s == "elseif", "")
                    exp <- parseExp
                    _ <- parseWord(_.s == "then", "")
                    block <- parseBlock
                yield
                    (exp, block)
            )
            else_ <- combOpt(
                for
                    _ <- parseWord(_.s == "else", "")
                    block <- parseBlock
                yield
                    block
            )
            _ <- parseWord(_.s == "end", "not a end")
        yield
            if elseIfList.length > 0 then
                val elseif = elseIfList.dropRight(1)
                .foldRight[NodeIf](
                    NodeIf(elseIfList.last._1, elseIfList.last._2, else_)
                )(
                    (expBlock, else_) => NodeIf(expBlock._1, expBlock._2, Some(NodeBlock(Seq(else_), None)))
                )
                NodeIf(exp, block, Some(NodeBlock(Seq(elseif), None)))
            else
                NodeIf(exp, block, else_)
                
    ) 
    val parseFor: ParseState[NodeForInt | NodeForExpr] = StateT(input => {
        takeNthToken(input, 1).flatMap((_, token) => {
            takeNthToken(input, 3).flatMap((_, token3) => {
                (token match
                    case Token("for", _) => 
                        token3 match
                            case Token("=", _) => parseForInt
                            case _ => parseForExpr
                    case _ => parseFail("not a for")).run(input)
            })
        })
    }) 
    val parseFunction: ParseState[NodeFunction] = (
        for
            _ <- parseWord(_.s == "function", "not a function")
            funcname <- parseFuncname
            funcbody <- parseFuncbody
        yield
            NodeFunction(funcname, funcbody)
    ) 
    val parseLocal: ParseState[NodeLocalFunction | NodeLocalNamelist] = StateT(input => {
        takeNthToken(input, 1).flatMap((_, token) => {
            takeNthToken(input, 2).flatMap((_, token2) => {
                (token match
                    case Token("local", _) => 
                        token2 match
                            case Token("function", _) => parseLocalFunction
                            case _ => parseLocalNamelist
                    case _ => parseFail("not a local")).run(input)
            })
        }) 
    })
    val parseAssFuncCall: ParseState[NodeFunctioncall | NodeVarlistAssignment] = StateT(input =>{
        parseFunctioncall.run(input) match
            case Right(value) => Right(value)
            case Left(value) => 
                parseVarlistAssignment.run(input)
    }) 

    val parseFuncname: ParseState[NodeFuncname] = (
        for
            name <- parseName
            namelist <- combList(
                for
                    _ <- parseWord(_.s == ".", "")
                    name <- parseName
                yield
                    name
            )
            self <- combOpt(parseName)
        yield
            NodeFuncname(name, namelist, self)
    )

    val parseForInt: ParseState[NodeForInt] = (
        for
            _ <- parseWord(_.s == "for", "not a for")
            name <- parseName
            _ <- parseWord(_.s == "=", "expected =")
            exp1 <- parseExp
            _ <- parseWord(_.s == ",", "expected ,")
            exp2 <- parseExp
            exp3Opt <- combOpt(
                for
                    _ <- parseWord(_.s == ",", "expected ,")
                    exp <- parseExp
                yield
                    exp)
            exp3 = exp3Opt match {case None => EndNodeNumeral(1L); case Some(value) => value}
            _ <- parseWord(_.s == "do", "not a do")
            block <- parseBlock
            _ <- parseWord(_.s == "end", "not a end")
        yield
            NodeForInt(name, exp1, exp2, exp3, block)
    )
    val parseForExpr: ParseState[NodeForExpr] = (
        for
            _ <- parseWord(_.s == "for", "not a for")
            namelist <- parseNamelist
            _ <- parseWord(_.s == "in", "not a in")
            explist <- parseExplist
            _ <- parseWord(_.s == "do", "not a do")
            block <- parseBlock
            _ <- parseWord(_.s == "end", "not a end")
        yield
            NodeForExpr(namelist, explist, block)
    )

    val parseLocalFunction: ParseState[NodeLocalFunction] = (
        for
            _ <- parseWord(_.s == "local", "not a local")
            _ <- parseWord(_.s == "function", "not a function")
            name <- parseName
            funcbody <- parseFuncbody
        yield
            NodeLocalFunction(name, funcbody)
    )
    val parseAttnamelist: ParseState[NodeAttnamelist] = {
        val attname: ParseState[(EndNodeName,Option[EndNodeName])] = (
            for
                name <- parseName
                att <- combOpt(
                    for 
                        _ <- parseWord(_.s == "<", "")
                        name <- parseName
                        _ <- parseWord(_.s == ">", "")
                    yield
                        name
                )
            yield
                (name, att)
        )
        for 
            name <- attname
            namelist <- combList(
                for 
                    _ <- parseWord(_.s == ",", "")
                    name <- attname
                yield
                    name
            )
        yield
            NodeAttnamelist(Seq(name)++namelist)
    }
    val parseLocalNamelist: ParseState[NodeLocalNamelist] = (
        for
            _ <- parseWord(_.s == "local", "not a local")
            namelist <- parseAttnamelist
            explist <- combOpt(
                for
                    _ <- parseWord(_.s == "=", "")
                    explist <- parseExplist
                yield
                    explist
            )
        yield
            NodeLocalNamelist(namelist, explist)
    )

    val parseVarlistAssignment: ParseState[NodeVarlistAssignment] = (
        for
            varlist <- parseVarlist
            _ <- parseWord(_.s == "=", "no assingment")
            explist <- parseExplist
        yield
            NodeVarlistAssignment(varlist, explist)
    )
}

@main
def test() = {
    import tokenizer.Tokenizer.tokenize
    // print(tokenize("::ad::").map(_.s))
    val a = Parser.parseBlock.run(tokenize("repeat return until a<b; a = b"))
    print(a.toString())
}