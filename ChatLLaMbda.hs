import Data.Char
import System.IO
import Control.Exception
import Data.List 
import Control.Monad

import LTypes
import qualified Lambek

import Parser
import PySupport
import LExp

-- "standardize" the capitalization of a word by making the first
-- letter uppercase and the following letters lowercase.
-- (This will make it easier to look up words in the lexicon.)
stdize (c:cs) = toUpper c : map toLower cs

-- EVALUATEMEANING: evaluates one interpretation in Python
evaluateMeaning :: PythonHandle -> [String] -> (LExp, [PythonCode]) -> IO String
evaluateMeaning phandle names (expr,meanings) = do 
        let wordMeanings = zip names (map text meanings)
        let pythonExpr = Lambek.toPython wordMeanings expr 
        runPythonCode phandle (PCode ("print(" ++ pythonExpr ++ ", flush=True)"))


-- read-eval-print-loop
repl :: Lexicon PythonCode -> PythonHandle -> IO ()
repl elx phandle = do
  -- print the prompt
  putStr "> "
  -- get a line of input
  line <- getLine
  -- split it up into "standardized" words
  -- let ws = map stdize $ words line
  -- FILTER OUT QUESTION MARKS FOR WHO VERSION
  let ws = map (stdize . filter (/= '?')) $ words line
  
  -- ORIGINAL CODE BEFORE SHOWING LAMBDA EXPRESSIONS
  -- test if the string of words is a grammatical sentence according to the lexicon
  -- if Lambek.isGrammatical elx ws
  --   then putStrLn "That is a well-formed sentence!"
  --   else putStrLn "That is not a well-formed sentence"
  -- END OF ORIGINAL CODE 

  -- NEW CODE TO DISPLAY LAMBDA EXPRESSIONS TO USER
  -- let derivations = Lambek.sentenceDerivations elx ws  
  -- if null derivations
  --  then putStrLn "That is not a well-formed sentence" 
  --  else do
  --      putStrLn "I can derive your sentence as follows:"
  --      mapM_ printLExp derivations
    
  -- repeat
  -- repl elx phandle

  -- NEW CODE FOR PART 3:
  -- let names = [w ++ "_" ++ show i | (w,i) <- zip ws [1..]]
  -- let interpretations = Lambek.sentenceMeanings elx ws 
  -- case interpretations of 
  --      [] -> putStrLn "That is not a well-formed sentence"
  --      [interpretation] -> do 
  --            result <- evaluateMeaning phandle names interpretation
  --            putStrLn result 
  --      _ -> do 
  --            putStrLn "That sentence is ambiguous"
  --            mapM_ (showInterpretation names) interpretations 
  -- repl elx phandle 
  -- where showInterpretation names interpretation = do 
  --                let (expr,_) = interpretation 
  --                result <- evaluateMeaning phandle names interpretation 
  --                putStrLn ( "Sense [" ++ prettyLExp expr ++ "]: " ++ result)

  -- UPDATED CODE FOR WHO QUESTION 
  case ws of 
    ("Who":rest) -> question elx phandle rest
    _ -> do 
          let names = [w ++ "_" ++ show i | (w,i) <- zip ws [1..]]
          let interpretations = Lambek.sentenceMeanings elx ws
          case interpretations of 
                [] -> putStrLn "That is not a well-formed sentence"
                [interpretation] -> do 
                        result <- evaluateMeaning phandle names interpretation
                        putStrLn result 
                _ -> do 
                        putStrLn "That sentence is ambiguous"
                        mapM_ (showInterpretation names) interpretations 
  repl elx phandle 
  where showInterpretation names interpretation = do 
                  let (expr,_) = interpretation 
                  result <- evaluateMeaning phandle names interpretation 
                  putStrLn ( "Sense [" ++ prettyLExp expr ++ "]: " ++ result)

      
main = do
  -- send console output immediately to the user
  hSetBuffering stdout NoBuffering
  -- input a lexicon file
  elx <- getLex
  -- input a database of facts
  db <- getDB
  -- launch a Python interpreter
  phandle <- launchPython db
  -- start the read-eval-print loop
  putStrLn "Starting chat..."
  repl elx phandle
  where
    getDB :: IO PythonCode
    getDB = do
      putStr "Facts about the world: "
      dname <- getLine
      mdb <- try (readFile dname) :: IO (Either IOError String)
      case mdb of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getDB
        Right db -> return (PCode db)
      
    getLex :: IO (Lexicon PythonCode)
    getLex = do
      putStr "Lexicon file: "
      lname <- getLine
      res <- parseFile lexicon lname
      case res of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getLex
        Right (Left err) -> putStrLn ("parse error at " ++ lname ++ ":" ++ show err) >> getLex
        Right (Right lex) -> return lex

-- QUESTION: answers the question "Who ____?" with all corresponding names
question :: Lexicon PythonCode -> PythonHandle -> [String] -> IO ()
question lex phandle rest = do 
      let candidates = nub [w | (w,Atm "np", _) <- items lex]
      answers <- filterM checkName candidates
      if null answers
        then putStrLn "Nobody"
        else putStrLn (intercalate ", " answers)

      where
        checkName name = do 
            let ws = name : rest
            let names = [w ++ "_" ++ show i | (w,i) <- zip ws [1..]]
            let interpretations = Lambek.sentenceMeanings lex ws
            results <- mapM (evaluateMeaning phandle names) interpretations
            return ("True" `elem` results)