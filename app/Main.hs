import Exec
import Lib
import System.Environment
import System.Exit
import System.IO
import System.IO.Error

main = do
  args <- getArgs
  case (length args > 0) of
    True -> do
      check <- checkArgs args
      case (check /= [] && length check == length args) of
        True -> do
          let transFormedToAst = transformToAst check
          let transform = transformAst transFormedToAst transFormedToAst 0
          let display = (displayAst (last transform) 0)
          printDisplay display
        False -> exitWith (ExitFailure 84)
    False -> do
      hPutStrLn stderr "ERROR: At least one argument must be given in order to execute this program"
      exitWith (ExitFailure 84)