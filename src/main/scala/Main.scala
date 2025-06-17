import tokenizer.Tokenizer.tokenize
import parser.Parser.parse
import interpreter.Interpreter.{executeChunk, emptyInt}

@main
def main() = {
    val test = "" +
        "a, b = 20, 45\n" +
        "while a ~= b do if a>b then a = a-b else b=b-a end end\n" +
        "return a"
    
    val tokens = tokenize(test)
    parse(tokens) match
        case Left(value) => print(value)
        case Right(chunk) => print(executeChunk(chunk).runA(emptyInt))
}