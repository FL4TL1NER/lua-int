package constants

val RESERVED_WORDS = Set(
    "and",
    "break",
    "do",
    "else",
    "elseif",
    "end",
    "false",
    "for",
    "function",
    "goto",
    "if",
    "in",
    "local",
    "nil",
    "not",
    "or",
    "repeat",
    "return",
    "then",
    "true",
    "until",
    "while"
    )

val EMPTY_TOKENS = Set(
    " ",
    "\n"
)

val DENOTERERS = Set(
    " ",
    "\n",
    "\"",
    "\'",
    "+", 
    "-", 
    "*", 
    "/", 
    "%", 
    "^", 
    "#", 
    "&", 
    "~", 
    "|", 
    "<<",
    ">>",
    "//",
    "==",
    "~=",
    "<=",
    ">=",
    "<", 
    ">", 
    "=", 
    "(",
    ")",
    "{",
    "}",
    "[",
    "]",
    "::",
    ";",
    ":",
    ",",
    ".",
    "..",
    "..."
    )

val ESCAPE_SEQUENCES = Set(
    "\\a",
    "\\b",
    "\\f",
    "\\n",
    "\\r",
    "\\t",
    "\\v",
    "\\",
    "\\\"",
    "\\\'",
    "\\z"
    )

val ASCII_LOWERCASE = "abcdefghijklmnopqrstuvwxyz"
val ASCII_UPPERCASE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
val NUMBERS_LETTERS = "1234567890"
