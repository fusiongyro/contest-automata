module Main where

import Control.Applicative
import System.Environment
import System.Console.GetOpt

import Automata

data Mode = Help 
          | Version 
          | GenCode 
          | GenGraphviz 
          | Eval FilePath
          | Test FilePath
  deriving (Show, Eq)

data Options = Options
  { optMode        :: Mode
  , optType        :: MachineType
  , optOutputFile  :: FilePath 
  , optInputFile   :: FilePath
  } deriving (Show, Eq)

defaultOptions = Options 
  { optMode        = Help
  , optType        = DFAType
  , optOutputFile  = "-"
  , optInputFile   = "-"
  }

version = "0.1"
usage = "Usage: contest-automata [OPTION...] input"

versionInfo = "contest-automata version " ++ version

-- | Set the mode of the option record
setMode :: Mode -> Options -> Options
setMode mode o = o { optMode = mode }

-- | Set the output filename of the option record
setOutput :: FilePath -> Options -> Options
setOutput fp o = o { optOutputFile = fp }

-- | Set the input filename of the option record
setInput :: FilePath -> Options -> Options
setInput fp o = o { optInputFile = fp }

-- | Set the type of the machine we're using
setType :: MachineType -> Options -> Options
setType ty o = o { optType = ty }

-- | The option descriptions
options :: [OptDescr (Options -> Options)]
options = 
  [ Option "v" ["version"]  (NoArg  (setMode Version))         "show version"
  , Option "h" ["help"]     (NoArg  (setMode Help))            "show this help"
  , Option "c" ["compile"]  (NoArg  (setMode GenCode))         "compile the machine"
  , Option "g" ["graphviz"] (NoArg  (setMode GenGraphviz))     "generate graphviz"
  , Option "e" ["eval"]     (ReqArg (setMode . Eval) "INPUT")  "evaluate input"
  , Option "t" ["test"]     (ReqArg (setMode . Test) "TESTS")  "test this case"
  , Option "o" ["output"]   (ReqArg  setOutput       "OUTPUT") "output file"
  , Option "m" ["type"]     (ReqArg (setType . read) "TYPE")   "machine type (default: DFA)"
  ]

-- | Parse the command line into an Option record based on the arguments
parseCommandLine :: IO Options
parseCommandLine = do
    argv <- getArgs
    case getOpt Permute options argv of
      (o, args, []) -> return $ finalize o args
      (_, _, errors) -> ioError $ userError $ concat errors ++ usageInfo usage options  
  where
    finalize o []      = foldl (flip ($)) defaultOptions o
    finalize o [input] = setInput input $ finalize o []

-- | Contents of the named file or standard input if the filename is "-"
getInput :: FilePath -> IO String
getInput "-"      = getContents
getInput filename = readFile filename

-- | Write string to named file or standard output if filename is "-"
writeOutput :: FilePath -> String -> IO ()
writeOutput "-"      = putStr
writeOutput filename = writeFile filename

-- | Read the machine from the specified input file. If successful, 
-- farm it off to a function that converts it to a string of some stripe. 
-- Take that and output it to the specified output file.
processMachine :: Options -> (Machine -> IO String) -> IO ()
processMachine (Options _ ty outf inf) f = do
  input <- getInput inf
  case parseMachine ty input of
    Left errs -> ioError $ userError errs
    Right m   -> f m >>= writeOutput outf

-- | Convert the machine of the input file into Haskell code in the output file
generateCode :: Options -> IO ()
generateCode opts = processMachine opts $ return . machineToHaskell

-- | Convert the machine of the input file into GraphViz output
generateGraphviz :: Options -> IO ()
generateGraphviz opts = processMachine opts $ return . graphMachine

-- | Run the machine with the specified machine input file. Output results.
evaluateCode :: Mode -> Options -> IO ()
evaluateCode (Eval testf) opts = 
    processMachine opts $ \m -> do
      testContent <- (map read . lines) <$> getInput testf
      return $ unlines $ map (acceptOrReject . evaluateMachine m) testContent
  where
    acceptOrReject x = if x then "Accept" else "Reject"

-- | Run the machine with the specified machine test file. If all the tests pass,
-- print True; otherwise print False
validateCode :: Mode -> Options -> IO ()
validateCode (Test verifyf) opts = 
  processMachine opts $ \m -> do
    testContent <- (map read . lines) <$> getInput verifyf
    let inputs  = map fst testContent
    let expects = map snd testContent
    return $ show (verifyMachine m inputs expects) ++ "\n"

main = do
  commandLine <- parseCommandLine
  main' commandLine
    where
      main'      (Options Version _ _ _)       = putStrLn versionInfo
      main'      (Options Help _ _ _)          = putStrLn $ usageInfo usage options
      main' opts@(Options GenCode _ _ _)       = generateCode opts
      main' opts@(Options GenGraphviz _ _ _)   = generateGraphviz opts
      main' opts@(Options mode@(Eval _) _ _ _) = evaluateCode mode opts
      main' opts@(Options mode@(Test _) _ _ _) = validateCode mode opts

