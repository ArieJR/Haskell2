import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Char as Char

--1a
runLengthEncode :: String -> String
runLengthEncode = concat . map (compressRun) . List.group

compressRun :: String -> String
compressRun g = (show . length $ g) ++ [head g]

--1b
runLengthDecode :: String -> String
runLengthDecode s = concat $ map (decomporessRun) compressedRuns
	where 
		compressedRuns = 
			Split.split 
				(Split.dropFinalBlank . Split.keepDelimsR $ Split.whenElt Char.isLetter)
				s

decomporessRun :: String -> String
decomporessRun compressedRun = 
	replicate (read $ init compressedRun) (last compressedRun)