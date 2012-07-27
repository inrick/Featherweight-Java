import System.Environment (getArgs)

import FJ.TypeCheck

main :: IO ()
main = do { args <- getArgs
          ; if (length args) > 0
                then runTypeCheck $ head args
                else putStrLn "Usage: runhaskell Main.hs <Featherweight Java file>"
          }

