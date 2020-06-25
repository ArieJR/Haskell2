module H2Opd1

where
import Data.Char
import Data.List

import System.IO  
import Control.Monad


	
	
nameLambda :: IO ()
nameLambda = putStr "compress" >>
             getLine >>= \ input ->
             putStr "to" >>
             getLine >>= \ compressed ->
             let file = "compressing" ++ input "to" ++ compressed
             in putStrLn (file)
