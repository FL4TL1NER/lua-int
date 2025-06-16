package tokenizer

import constants.*

object Tokenizer {
    def tokenize(input: String): Seq[Token] = {
    def tokenizeChar(tokens: Seq[String], char: Char): Seq[String] = {
        if DENOTERERS.contains(tokens.last + char.toString()) then {
            tokens.slice(0,tokens.length-1):+(tokens.last + char.toString())
        } else if DENOTERERS.contains(char.toString()) then {
            tokens:+char.toString()
        }
        else {
            if !DENOTERERS.contains(tokens.last) then {
                tokens.slice(0,tokens.length-1):+(tokens.last + char.toString())
            } else {
                tokens:+char.toString()
            }
        }
    }

    def tokenType(s: String): TokenType = {
        if EMPTY_TOKENS.contains(s) then {
            TokenType.empty
        } else if DENOTERERS.contains(s) then {
            TokenType.denoter
        } else if RESERVED_WORDS.contains(s) then {
            TokenType.reservedWord
        } else if s.filter((a) => !(ASCII_LOWERCASE.contains(a) || ASCII_UPPERCASE.contains(a) || NUMBERS_LETTERS.contains(a))).isEmpty then {
            if NUMBERS_LETTERS.contains(s.head) then TokenType.number else TokenType.name
        } else {
            TokenType.string
        }
    }

    input.foldLeft(Seq(""))(tokenizeChar).map((s) => Token(s,tokenType(s)))
    }

    enum TokenType:
        case empty
        case denoter
        case reservedWord
        case name
        case number
        case string

    case class Token(s: String, t: TokenType)
}
