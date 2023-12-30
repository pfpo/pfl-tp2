import Parser
import Compiler
import Runner

-- Function to test the assembler (run function in Runner.hs)
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

-- Function to test all parts of the program (run function in Runner.hs, compile function in Compiler.hs and parse function in Parser.hs)
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run (compile (parse programCode), createEmptyStack, createEmptyState)
