package parser

import scala.annotation.tailrec

import tokenizer.Tokenizer.Token
import tokenizer.Tokenizer.TokenType
import constants.*

import cats.data.StateT

object ParserUtils {
    // @tailrec//Cannot rewrite recursive call: it is not in tail positionbloop
    // def takeNthToken(input: Seq[Token], n: Int): Either[String, (Seq[Token], Token)] = {
    //     input.headOption.toRight("no tokens").flatMap(token => {
    //         token match
    //             case Token(_, TokenType.empty) => takeNthToken(input.tail, n)
    //             case _ => if n <= 1 then Right(input.tail, input.head) else takeNthToken(input.tail, n-1)
    //     }) 
    // }

    @tailrec
    def takeNthToken(input: Seq[Token], n: Int): Either[String, (Seq[Token], Token)] = {
        input.headOption match 
            case None => Left("No tokens")
            case Some(token) => 
                token match
                    case Token(_, TokenType.empty) => takeNthToken(input.tail, n)
                    case _ => if n <= 1 then Right(input.tail, input.head) else takeNthToken(input.tail, n-1) 
    }

    def parseWord(tokenPred: Token => Boolean, error: String): ParseState[Token] = StateT(
        input => {
            takeNthToken(input, 1).flatMap((tokens, token) => if tokenPred(token) then Right(tokens, token) else Left(error))
        }
    )

    def parseNextToken(tokenPred: Token => Boolean, error: String): ParseState[Token] = StateT(
        input => {
            input.headOption.toRight("no tokens")
            .flatMap(token => if tokenPred(token) then Right(input.tail, token) else Left(error))
        }
    )

    // def combOpt[A](pred: ParseState[_], state: ParseState[A]): ParseState[Option[A]] = {
    //     StateT(input => {
    //         state.run(input) match
    //             case Left(_) => Right(input, None)
    //             case Right(tokens: Seq[Token], a: A) => Right(tokens, Some(a))
    //     })
    // }

    def combOpt[A](pred: Seq[Token] => Boolean, state: ParseState[A]): ParseState[Option[A]] = {
        StateT(input => {
            if pred(input) then
                state.run(input).map((state, a) => (state, Some(a)))
            else
                Right(input, None)
        })
    }

    // def combList[A](state: ParseState[A]): ParseState[Seq[A]] = {
    //     @tailrec
    //     def combListRec(tokens: Seq[Token], res: Seq[A]): (Seq[Token], Seq[A]) = {
    //         state.run(tokens).toOption match
    //             case None => (tokens, res)
    //             case Some(tokens: Seq[Token], a) => combListRec(tokens, res++Seq(a))
    //     }

    //     StateT[parseRes, Seq[Token], Seq[A]](input => {
    //         Right(combListRec(input, Seq.empty[A]))
    //     })
    // }

    def combList[A](pred: Seq[Token] => Boolean, state: ParseState[A]): ParseState[Seq[A]] = {
        @tailrec
        def combListRec(tokens: Seq[Token], res: Seq[A]): parseRes[(Seq[Token], Seq[A])] = {
            if pred(tokens) then
                state.run(tokens) match //flatmap не подходит для устранения левой рекурсии
                    case Left(s) => Left(s)
                    case Right(tokens, a) => combListRec(tokens, res:+a)
            else
                Right(tokens, res)        
        }

        StateT(input => {
            combListRec(input, Seq.empty[A])
        })
    }

    def parseFail[A](error: String): ParseState[A] = {
        StateT[parseRes, Seq[Token], A](input => {
            Left(error)
        })
    }

    def wordPred(pred: Token => Boolean): Seq[Token] => Boolean = (
        input => parseWord(pred(_), "").run(input).isRight
    )
}
