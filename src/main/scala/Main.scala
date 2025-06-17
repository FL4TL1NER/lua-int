import tokenizer.Tokenizer.tokenize
import parser.Parser.parse
import interpreter.Interpreter.{executeChunk, emptyInt}

@main
def main() = {
    val test = "" +
        "hello, world = \"Hello\", \"world\"\n" +
        "return hello..\" \"..world"
    
    println()
    val tokens = tokenize(test)
    parse(tokens) match
        case Left(value) => print(value)
        case Right(chunk) => print(executeChunk(chunk).runA(emptyInt))
    println()
}