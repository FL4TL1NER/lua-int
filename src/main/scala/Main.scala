import scala.annotation.tailrec
import scala.Boolean
import scala.compiletime.ops.int
import scala.collection.immutable.NumericRange

import tokenizer.Tokenizer.tokenize
//import parser.Parser.parse

@main
def main() = {
    // val another_lua_string = "" +
    //     "--comment\n" +
    //     "--[[\n" +
    //     "\n" +
    //     "--]]\n" +
    //     "print('rtx 2070s')\n"
    // println("Initial string:")
    // print(another_lua_string)
    // println("Preprocessed string:")
    // val preprocessed = preprocess(another_lua_string)
    // print(preprocessed)
    // val tokens = tokenize(preprocessed)
    val tokens = tokenize("print('rtx 2070s')")
    // tokenize("print('rtx 2070s')").map(_.s).foreach((a) => (print("\""),print(a),print("\", ")))
    print("\n\n")
    // tokenize("print()").foreach((a) => (print("\""),print(a),print("\", ")))
    //parse(tokens).foreach((a) => print(a.toString()))
    // val lua_string = "print(\"hello world\")"
    // tokenize(lua_string).foreach((a) => (print("\""),print(a),print("\", ")))
    // print(DENOTERERS.contains("("))
}