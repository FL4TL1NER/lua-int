import tokenizer.Tokenizer.tokenize
import parser.Parser.parse
import interpreter.Interpreter.{executeChunk, emptyInt}

@main
def main() = {
    val test = "" +
        "hello, world = \"Hello\", \"world\"\n, \"nigga\", x" +
        "return hello..\" \"..world"
    
    println()
    val tokens = tokenize(test)
    parse(tokens) match
        case Left(value) => print("Error: " + value)
        case Right(chunk) => 
            // print(chunk.block)
            // println()
            print(executeChunk(chunk).runA(emptyInt))
    println()
    println()
}