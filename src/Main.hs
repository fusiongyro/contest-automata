module Main where

import System.Environment
import System.Console.GetOpt

import Automata.DFA

data Mode = Help 
          | Version 
          | GenCode 
          | GenGraphviz 
          | Eval FilePath
          | Test FilePath
  deriving (Show, Eq)

data Options = Options
  { optMode        :: Mode
  , optOutputFile  :: FilePath 
  , optInputFile   :: FilePath
  } deriving (Show, Eq)

defaultOptions = Options 
  { optMode        = Help
  , optOutputFile  = "-"
  , optInputFile   = "-"
  }

version = "0.1"
usage = "Usage: contest-automata [OPTION...] input"

versionInfo = "contest-automata version " ++ version

setMode :: Mode -> Options -> Options
setMode mode o = o { optMode = mode }

setOutput :: FilePath -> Options -> Options
setOutput fp o = o { optOutputFile = fp }

setInput :: FilePath -> Options -> Options
setInput fp o = o { optInputFile = fp }

options :: [OptDescr (Options -> Options)]
options = 
  [ Option "v" ["version"]  (NoArg  (setMode Version))         "show version"
  , Option "h" ["help"]     (NoArg  (setMode Help))            "show this help"
  , Option "c" ["compile"]  (NoArg  (setMode GenCode))         "compile the DFA"
  , Option "g" ["graphviz"] (NoArg  (setMode GenGraphviz))     "generate graphviz"
  , Option "e" ["eval"]     (ReqArg (setMode . Eval) "INPUT")  "evaluate input"
  , Option "t" ["test"]     (ReqArg (setMode . Test) "TESTS")  "test this case"
  , Option "o" ["output"]   (ReqArg  setOutput       "OUTPUT") "output file"
  ]

parseCommandLine :: IO Options
parseCommandLine = do
    argv <- getArgs
    case getOpt Permute options argv of
      (o, args, []) -> return $ finalize o args
      (_, _, errors) -> ioError $ userError $ concat errors ++ usageInfo usage options  
  where
    finalize o []      = foldl (flip id) defaultOptions o
    finalize o [input] = setInput input $ finalize o []

getInput :: FilePath -> IO String
getInput "-"      = getContents
getInput filename = readFile filename

writeOutput :: FilePath -> String -> IO ()
writeOutput "-"      = putStr
writeOutput filename = writeFile filename

generateCode :: Options -> IO ()
generateCode (Options _ outf inf) = do
  input <- getInput inf
  case parseDFA input of
    Left errs -> ioError $ userError $ show errs
    Right dfa -> writeOutput outf $ dfaToHaskell dfa

main = do
  commandLine <- parseCommandLine
  main' commandLine
    where
      main'      (Options Version _ _)  = putStrLn $ versionInfo
      main'      (Options Help _ _)     = putStrLn $ usageInfo usage options
      main' opts@(Options GenCode _ _)  = generateCode opts
      main' _                           = putStrLn $ "sorry, this option is not implemented!"
