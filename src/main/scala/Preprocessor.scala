import scala.annotation.tailrec

object Preprocessor {
    @tailrec
    def preprocess(input: String): String = {
        def sliceOut(s: String,startIndex: Int, endIndex: Int): String = {
            s.slice(0,startIndex) + s.slice(endIndex,input.length)
        }
    
        if !input.contains("--") then {
            input
        } else {
            if !input.contains("--[[") then {
                val startIndex = input.indexOf("--")
                val endIndex = input.indexOf("\n",startIndex+2) + 1
    
                if endIndex != -1 then
                    preprocess(sliceOut(input,startIndex,endIndex))
                else
                    input.slice(0,startIndex)
            } else {
                val startIndex = input.indexOf("--[[")
                val endIndex = input.indexOf("]]",startIndex+4) + 2
    
                if endIndex != -1 then
                    preprocess(sliceOut(input,startIndex,endIndex))
                else
                    input.slice(0,startIndex)
            }
        }
    }
}
