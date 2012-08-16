module Data.String.Builder where

import Data.List
import Control.Monad.Writer

type Builder = Writer [String]

writeLn :: String -> Builder ()
writeLn x = write $ x ++ "\n"

write :: String -> Builder ()
write x = tell [x]

writeLines :: [String] -> Builder ()
writeLines = write . unlines 

indent :: Builder () -> Builder ()
indent b = write $ unlines $ map ("  "++) $ lines $ buildString b

newline :: Builder ()
newline = write "\n"

buildString :: Builder () -> String
buildString b = concat w 
  where
    (_, w) = runWriter b