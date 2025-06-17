package parser

import scala.util.{Try, Success, Failure}

import cats.data.StateT

import tokenizer.Tokenizer.{Token, TokenType}
import nodes.Nodes.*
import ParserUtils.*
import parser.Parser.{parseBlock, parseNamelist, parseName}

object ParserExp {
    val parseNumber: StateT[parseRes, Seq[Token], EndNodeNumeral] = StateT(input => {
        val parseString: ParseState[String] = (
            for
                n1 <- parseWord(_.t == TokenType.number,"not a number")
                dotn2 <- combOpt(
                    for
                        dot <- parseNextToken(_.s == ".", "not a dot")
                        n2 <- parseNextToken(_.t == TokenType.number,"not a number")
                    yield
                        Seq(dot, n2)
                )
            yield
                dotn2 match
                    case None => n1.s
                    case Some(value) => (Seq(n1)++value).map(_.s).reduce(_ + _)
        )

        parseString.run(input).map((tokens, str) => {
            if str.contains(".") | str.contains("e") then
                (tokens, Try(str.toDouble))
            else
                (tokens, Try(str.toLong))
        })
        .flatMap((tokens, number) => {
            number match
                case Success(value) => Right((tokens, EndNodeNumeral(value)))
                case Failure(exception) => Left(exception.toString())
        })
    })

    val parseLiteralString: StateT[parseRes, Seq[Token], EndNodeLiteralString] = StateT(input => {
        def parseOneLineString(input: Seq[Token]): Either[String, (Seq[Token], EndNodeLiteralString)] = {
            takeNthToken(input, 1)
            .flatMap((rest, token) => {
                token match
                case Token("\"", _) =>
                    val last_ind = rest.map(_.s).indexOf("\"")
                    val next_line = rest.map(_.s).indexOf("\n")
                    if last_ind == -1 then
                        Left("unclosed \"")
                    else if (last_ind > next_line) & next_line!=(-1) then
                        Left("unclosed \"")
                    else
                        Right((
                            rest.slice(last_ind+1, rest.length),
                            EndNodeLiteralString(
                                rest.slice(0,last_ind).map(_.s).reduce(_ + _)
                            )
                        ))
                case _ => Left("not a string")
            })
        }

        takeNthToken(input, 1).flatMap({(_, token) => {token match
            case Token("\"", _) => parseOneLineString(input)
            case Token("[", _) => Left("WIP")
            case Token(s, _) => Left(s"${s} is not a string")}}) 
    })


    def makeParseBinOp(binOpSet: Set[String], nextParse: StateT[parseRes, Seq[Token], NodeExp]): StateT[parseRes, Seq[Token], NodeExp] = {
        for
            exp1 <- nextParse
            expList <- combList[(BinOp, NodeExp)](
                for
                    binop <- (
                        parseWord(t => binOpSet.contains(t.s), "not in binOpSet")
                        .map(t => BinOp.find(t.s) match {case Some(value) => value})
                    )
                    exp2 <- nextParse
                yield
                    (binop, exp2)
            )
        yield
            expList.foldLeft(exp1)((exp1, binopexp2) => NodeBinOpExp(binopexp2._1,exp1,binopexp2._2))
    }

    val parseSimpleExp = StateT[parseRes, Seq[Token], NodeExp](input => {
        takeNthToken(input, 1).flatMap((_, token) => 
            (token match
                case Token("nil", _) => parseNil
                case Token("true", _) | Token("false", _) => parseBoolean
                case Token(_, TokenType.number) => parseNumber
                case Token("\"", _) | Token("[", _) => parseLiteralString
                case Token("...", _) => parseVararg
                case Token("function", _) => parseFunctiondef
                case Token(_, TokenType.name) | Token("(", _) => parseValue
                case Token("{", _) => parseTableconstructor
                case _ => parseFail("not an expression")).run(input))
    })
   
    val parseBinExp10 = makeParseBinOp(Set("^"), parseSimpleExp)
    val parseUnExp: StateT[parseRes, Seq[Token], NodeExp] = (
        for 
            unOp <- combOpt(
                parseWord(t => Set("-", "not", "#", "~").contains(t.s), "")
                .map(t => UnOp.find(t.s) match {case Some(value) => value}))
            exp <- parseBinExp10
        yield
            unOp match
                case Some(unOp) => NodeUnOpExp(unOp, exp)
                case None => exp
    )
    val parseBinExp9 = makeParseBinOp(Set("*", "/", "//", "%"), parseUnExp)
    val parseBinExp8 = makeParseBinOp(Set("+", "-"), parseBinExp9)
    val parseBinExp7 = makeParseBinOp(Set(".."), parseBinExp8)
    val parseBinExp6 = makeParseBinOp(Set("<<", ">>"), parseBinExp7)
    val parseBinExp5 = makeParseBinOp(Set("&"), parseBinExp6)
    val parseBinExp4 = makeParseBinOp(Set("~"), parseBinExp5)
    val parseBinExp3 = makeParseBinOp(Set("<", ">", "<=", ">=", "~=", "=="), parseBinExp4)
    val parseBinExp2 = makeParseBinOp(Set("and"), parseBinExp3)
    val parseExp = makeParseBinOp(Set("or"), parseBinExp2)

    val parseNil = parseWord(_.s == "nil", "not a nil").map(_ => NodeNil())
    val parseBoolean = (
        parseWord(t => Set("true", "false").contains(t.s), "not a boolean")
        .map(t => {t.s match {case "true" => NodeBoolean(true); case "false" => NodeBoolean(false);}})
    )
    val parseVararg = parseWord(_.s == "...", "not a ...").map(_ => NodeVararg())

    val parseFunctiondef = (
        for
            word <- parseWord(_.s == "function", "not a function")
            funcbody <- parseFuncbody
        yield
            NodeFunctiondef(funcbody)
    )
    val parseFuncbody = (
        for
            _ <- parseWord(_.s == "(", "no (")
            parlist <- combOpt(parseParList)
            _ <- parseWord(_.s == ")", "no )")
            block <- parseBlock
            _ <- parseWord(_.s == "end", "no end")
        yield
            NodeFuncbody(parlist, block)
    )
    val parseParList = StateT[parseRes, Seq[Token], NodeParlist](input => {
        val parseParListNamelist = (
            for
                namelist <- parseNamelist
                varargOpt <- combOpt(
                    for
                        _ <- parseWord(_.s == ",", "")
                        vararg <- parseVararg
                    yield
                        vararg
                )
            yield
                NodeParlistNamelist(namelist, varargOpt))

        takeNthToken(input, 1).flatMap((_, token) => 
            (token match
                case Token("...", _) => parseVararg
                case Token(_, TokenType.name) => parseParListNamelist
                case _ => parseFail("not a parameter")).run(input)
        )
    })

    val parseValue: StateT[parseRes, Seq[Token], NodeValue] = {
        val parseValueHead = StateT[parseRes, Seq[Token], EndNodeName | NodeExp](input => {
            val parseBrackets = (
                for
                    _ <- parseWord(_.s == "(", "")
                    exp <- parseExp
                    _ <- parseWord(_.s == ")", "")
                yield
                    exp
            )

            takeNthToken(input, 1).flatMap((_, token) => 
                (token match
                    case Token("(", _) => parseBrackets
                    case Token(_, TokenType.name) => parseName
                    case _ => parseFail("not a value")).run(input)
            )
        })
        val parseValueTail = StateT[parseRes, Seq[Token], NodeVar_index | NodeFunctioncall_args](input =>
            takeNthToken(input, 1).flatMap((_, token) => 
                (token match
                    case Token(".", _)=> parseVar_index
                    case Token("[", _) =>
                        takeNthToken(input, 2).map((_, token2) => 
                            token2 match
                                case Token("=", _) => parseFunctioncall_args
                                case _ => parseVar_index
                        ) match //вообще не нравится этот кусок ****
                            case Left(value) => parseFail(value)
                            case Right(value) => value
                    case Token(":", _) | Token("(", _) | Token("{", _) | Token("\"", _) => parseFunctioncall_args
                    case _ => parseFail("not a tail")).run(input)
            )
        )
        
        for
            head <- parseValueHead
            tail <- combList(parseValueTail)
        yield
            NodeValue(head, tail)
    }
    val parseVar_index = StateT[parseRes, Seq[Token], NodeVar_index](input => {
        val parseVar_index1 = (
            for
                _ <- parseWord(_.s == "[", "")
                exp <- parseExp
                _ <- parseWord(_.s == "]", "")
            yield
                NodeVar_index(exp)
        )
        val parseVar_index2 = (
            for
                _ <- parseWord(_.s == ".", "")
                name <- parseWord(_.t == TokenType.name, "").map(t => EndNodeLiteralString(t.s))
            yield
                NodeVar_index(name)
        )

        takeNthToken(input, 1).flatMap((_, token) =>
            (token match
                case Token("[", _) => parseVar_index1
                case Token(".", _) => parseVar_index2
                case _ => parseFail("")).run(input))
    })
    val parseFunctioncall_args = (
        for
            name <- combOpt(
                for
                    _ <- parseWord(_.s == ":", "")
                    name <- parseName
                yield
                    name
            )
            args <- parseArgs
        yield
            NodeFunctioncall_args(name, args)
    )
    val parseArgs = StateT[parseRes, Seq[Token], NodeArgs](input => {
        val parseArgs1 = (
            for
                _ <- parseWord(_.s == "(", "")
                explist <- combOpt(parseExplist)
                _ <- parseWord(_.s == ")", "")
            yield
                NodeArgsBracets(explist)
        )

        takeNthToken(input, 1).flatMap((_, token) =>
            (token match
                case Token("(", _) => parseArgs1
                case Token("\"", _) => parseLiteralString
                case Token("[", _) =>
                    takeNthToken(input, 2).map((_, token2) => 
                        token2 match
                            case Token("=", _) => parseLiteralString
                            case _ => parseFail("not a string")
                    ) match
                        case Left(value) => parseFail(value)
                        case Right(value) => value
                case Token("{", _) => parseTableconstructor
                case _ => parseFail("not an arg")
        ).run(input))
    })

    val parseTableconstructor = (
        for
            _ <- parseWord(_.s == "{", "not a tableconstructor")
            fields <- combOpt(fieldList)
            _ <- parseWord(_.s == "}", "unclosed }")
            fieldlist = fields match {case None => Seq.empty[NodeField]; case Some(value) => value}
        yield
            NodeTableconstructor(fieldlist)
    )
    val parseField = StateT[parseRes, Seq[Token], NodeField](input => {
        val parseFieldExpExp = (
            for
                _ <- parseWord(_.s == "[", "")
                exp1 <- parseExp
                _ <- parseWord(_.s == "]", "")
                _ <- parseWord(_.s == "=", "")
                exp2 <- parseExp
            yield
                NodeFieldExpExp(exp1, exp2)
        )
        val parseFieldNameExp = (
            for
                name <- parseWord(_.t == TokenType.name, "").map(t => EndNodeLiteralString(t.s))
                _ <- parseWord(_.s == "=", "")
                exp2 <- parseExp
            yield
                NodeFieldExpExp(name, exp2)
        )

        takeNthToken(input, 1).flatMap((_, token) =>
            (token match
                case Token("[", t) => parseFieldExpExp
                case Token(_, TokenType.name) =>
                    takeNthToken(input, 2).map((_, token2) =>
                        token2 match
                            case Token("=", _) => parseFieldNameExp
                            case _ => parseExp
                        ) match
                            case Left(value) => parseFail(value)
                            case Right(value) => value 
                case _ => parseExp).run(input))
        // takeNthToken(input, 1).flatMap((_, token) =>
        // token match
        //     case Token("[", _) => parseFieldExpExp.run(input)
        //     case Token(_, TokenType.name) =>
        //         takeNthToken(input, 2).flatMap((_, token2) =>
        //             token2 match
        //                 case Token("=", _) => parseFieldNameExp.run(input)
        //                 case _ => parseExp.map(exp => NodeFieldExp(exp)).run(input))
        //     case _ => parseExp.map(exp => NodeFieldExp(exp)).run(input))
    })
    val fieldList: StateT[parseRes, Seq[Token], Seq[NodeField]] = (
        for
            field <- parseField
            fieldList <- combList(
                for
                    _ <- parseWord(t => Set(",", ";").contains(t.s), "")
                    field <- parseField
                yield
                    field
            )
            _ <- combOpt(parseWord(t => Set(",", ";").contains(t.s), ""))
        yield
            Seq(field)++fieldList
    )

    val parseExplist: StateT[parseRes, Seq[Token], NodeExplist] = (
        for
            exp <- parseExp
            explist <- combList(
                for
                    _ <- parseWord(_.s == ",", "")
                    exp <- parseExp
                yield
                    exp
            )
        yield
            NodeExplist(Seq(exp)++explist)
    )

    val parseVar = StateT[parseRes, Seq[Token], NodeVar](input => {
        parseValue.run(input)
        .flatMap((tokens, value) => {
            if (value.start match {case EndNodeName(_) => true; case _ => false}) && (value.tail.isEmpty) then
                Right((tokens, NodeVar(value)))
            else if value.tail.last match {case NodeVar_index(_) => true; case _ => false} then
                Right((tokens, NodeVar(value)))
            else
                Left("not a variable")
        })
    })

    val parseFunctioncall = StateT[parseRes, Seq[Token], NodeFunctioncall](input => {
        parseValue.run(input)
        .flatMap((tokens, value) => {
            value.tail.lastOption match
                case Some(value2) => 
                    if value2 match {case NodeFunctioncall_args(_, _) => true; case _ => false} then
                        Right((tokens, NodeFunctioncall(value)))
                    else
                        Left("not a function call")
                case None => Left("not a function call")
        })
    })
}

// @main
// def test() = {
//     import tokenizer.Tokenizer.tokenize
//     // print(tokenize(""" " 21313 34 1488 dasda" """).map(_.s))
//     val a = ParserExp.parseValue.run(tokenize("name\"148\""))
//     print(a.toString())
// }