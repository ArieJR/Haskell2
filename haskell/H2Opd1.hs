module H2Opd1

where
import Data.Char
import Data.List

import System.IO  
import Control.Monad

main = do
    let file = "data.txt"
    contents <- readFile file
    putStrLn contents
    let filecomp = "compressed.txt"
    writeFile filecomp "what"
    contentsC <- readFile filecomp
    putStrLn contentsC