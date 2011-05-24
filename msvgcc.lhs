#!/usr/bin/env runhaskell

# Originally written by Turo Lamminen.
# This code is in the Public Domain and comes with ABSOLUTELY NO WARRANTY. If it breaks you get to keep all the pieces.

\begin{code}
{- OPTIONS_GHC  -Wall -Werror -}

module Main (main) where
import Control.Monad (filterM)
import Data.Char (isSpace)
import Data.List (intercalate, intersperse, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Set as Set (Set)
import qualified Data.Set as Set(
                         deleteFindMin
                       , difference
                       , empty
                       , fromList
                       , insert
                       , member
                       , null
                       , singleton
                       , toList
                       , union)
import System.Console.GetOpt
import System.Directory (doesFileExist
                       , canonicalizePath
                       , getCurrentDirectory
                       , makeRelativeToCurrentDirectory)
import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.Process


data Option
 = CompileOnly
 | Define String
 | DependencyFile String      -- name of dependency file
 | DependencyGenerate
 | DependencyPhonyHeaders
 | DependencyTarget String    -- name of target in dependency file
 | IncludeDir String
 | OptFlag String
 | OutputFile String
   deriving Show


optDescription :: [OptDescr Option]
optDescription =
  [ Option "c" [] (NoArg CompileOnly) "Compile only"
  , Option "f" [] (ReqArg OptFlag "flag") "Optimization flag"
  , Option "D" [] (ReqArg Define "def")     "Preprocessor define"
  , Option "I" [] (ReqArg IncludeDir "dir") "Include directory"
  , Option "M" [] (ReqArg depGen "dep") "Dependency generation"
  , Option "o" [] (ReqArg OutputFile "out") "Output file"
  ]


depGen :: String -> Option
depGen ('T':str) = DependencyTarget str
depGen ('F':str) = DependencyFile str
depGen ('P':_) = DependencyPhonyHeaders
depGen ('M':'D':_) = DependencyGenerate
depGen str = error $ "Unknown dep: " ++ str


data CompileMode
 = Compile
 | CompileAndLink
   deriving (Eq)


data MsvcOptions
 = MsvcOptions {
   mode :: CompileMode
 , generateDeps
 , depPhonyHeaders   :: Bool
 , depTarget
 , depFile  :: String
 , clOptions
 , includeDirs :: [String]
 }


defaultMsvcOptions :: MsvcOptions
defaultMsvcOptions = MsvcOptions CompileAndLink False False "" "" [] []


addOpt :: String -> MsvcOptions -> MsvcOptions
addOpt newOpt oldOpts = oldOpts { clOptions = ((clOptions oldOpts) ++ [newOpt]) }


snoc :: [a] -> a -> [a]
snoc [] x = x:[]
snoc (x1:xs) x2 = x1:(snoc xs x2)


translateOption :: Option -> MsvcOptions -> MsvcOptions
translateOption CompileOnly            oldOpts = (addOpt "/c" oldOpts) { mode = Compile }
translateOption (Define str)           oldOpts = addOpt ("/D" ++ str) oldOpts
translateOption (DependencyFile f)     oldOpts = oldOpts { depFile = f }
translateOption DependencyGenerate     oldOpts = oldOpts { generateDeps = True }
translateOption DependencyPhonyHeaders oldOpts = oldOpts { depPhonyHeaders = True }
translateOption (DependencyTarget f)   oldOpts = oldOpts { depTarget = f }
translateOption (IncludeDir dir)       oldOpts = oldOpts { includeDirs = (snoc (includeDirs oldOpts) dir) }
translateOption (OptFlag flag)         oldOpts = oldOpts  -- FIXME: do something interesting with this


-- msvc has different option for object and exe output name...
translateOption (OutputFile out) oldOpts =
  addOpt (newOpt ++ out) oldOpts
  where
    newOpt =
      case mode oldOpts of
        Compile        -> "/Fo"
        CompileAndLink -> "/Fe"

-- translateOption str@_ _ = error $ "fuck " ++ (show str)
-- translateOption _ oldOpts = oldOpts


generateDependecyFile :: String -> MsvcOptions -> IO ()
generateDependecyFile srcFile opts = do
  if not $ generateDeps opts
    then return ()   -- no deps requested
    else do let targetName = depTarget opts
            let file = depFile opts
            
            includes <- recursiveIncludes (includeDirs opts) srcFile
            
            let deps =
                 -- the main target
                 targetName ++ ": " ++ (intercalate " " (Set.toList includes))
                  ++ "\n"
                   -- empty target for all include files
                   -- to avoid build errors when include files deleted
                  ++ (concatMap (\x -> x ++ ":\n") (Set.toList includes))
            
            writeFile file deps


-- recursively find files included by a file
-- recursiveIncludes includeDirs file (set of includes)
recursiveIncludes :: [String] -> String -> IO (Set String)
recursiveIncludes includeDirectories = do
  recursiveIncludes' Set.empty . Set.singleton
  where
    recursiveIncludes' :: Set String -> Set String -> IO (Set String)
    recursiveIncludes' accum todo = do
      -- if todo has become empty, we're done
      if Set.null todo
        then return accum
        else do
          let (filename, newTodo) = Set.deleteFindMin todo
          
          -- read the raw includes
          -- contains filenames without full path
          -- but might have relative paths
          rawIncludes <- readIncludes filename
          
          -- resolve with the help of includeDirs
          -- also need to add path of file being currently processed
          -- and the current working dir
          realIncludes <- mapM (findRealInclude (".":(takeDirectory filename):includeDirectories)) $ Set.toList rawIncludes
          
          -- check we found all we were looking for
          (includes0, errors) <- checkIncludes realIncludes
          
          -- if errors, exit immediately
          -- FIXME: collect all errors and exit at the end
          if not $ null errors
            then do putStrLn $ "recursiveIncludes error while processing " ++ filename ++  ": "
                    mapM_ putStrLn errors
                    exitFailure
            else return ()
          
          -- see if any new
          let newIncludes = Set.difference includes0 accum
          let newAccum = Set.union accum newIncludes
          
          -- recurse
          recursiveIncludes' newAccum (Set.union newTodo newIncludes)

-- look through include paths and find the full path of an include
-- returns (raw include read from file, path to it)
findRealInclude :: [String] -> String -> IO (String, [String])
findRealInclude includeDirectories includeFileName = do
  let potentialNames = map (\x -> x ++ ('/':includeFileName)) includeDirectories
  filesThatExist <- filterM doesFileExist potentialNames
  return (includeFileName, filesThatExist)

-- takes a filename and list of possible directories
-- returns a set of files (with path) and a list of errors
-- if list nonempty, error occurred
checkIncludes :: [(String, [String])] -> IO (Set String, [String])
checkIncludes =
  checkIncludes' (Set.empty, [])
  where
    checkIncludes' :: (Set String, [String]) -> [(String, [String])] -> IO (Set String, [String])
    checkIncludes' accum [] = return accum
    
    -- includename = short include name read from file
    -- includePaths = candidates for the real location
    checkIncludes' (includeAccum, errorAccum) ((includeName, includePaths):xs) = do
      -- check that includePaths is not empty
      case includePaths of
        []              -> checkIncludes' (includeAccum, ("no candidates found for " ++ includeName):errorAccum) xs
        -- grab the first as path
        (includePath:_) -> checkIncludes' (Set.insert includePath includeAccum, errorAccum) xs


-- find files included by a file
readIncludes :: String -> IO (Set String)
readIncludes cppFile = do
  contents <- readFile cppFile
  return $ parseIncludes contents
  where
    parseIncludes :: String -> Set String
    parseIncludes str =
      Set.fromList includeLines
      where
        l = lines str
        includeLines = mapMaybe isIncludeLine l
        isIncludeLine str = do
          tmp1 <- stripPrefix "#include" str
          let tmp2 = dropWhile isSpace tmp1
          case tmp2 of
            '"':str -> return $ takeWhile (/= '"') str
            _       -> Nothing


dropCommonPrefix :: Eq a => [a] -> [a] -> ([a], [a])
dropCommonPrefix [] ys = ([], ys)
dropCommonPrefix xs [] = (xs, [])
dropCommonPrefix (x:xs) (y:ys) =
  if x == y
    then dropCommonPrefix xs ys
    else (x:xs, y:ys)


-- WTF? not in any lib?
-- currentPath newFile
realMakeRelative :: String -> String -> String
realMakeRelative path filename =
  -- drop the common prefix
  let (newPath, newFilename) = dropCommonPrefix path filename
      splitNewPath = splitPath newPath
      splitNewFilename = splitPath newFilename
      droppedPathComponents = length splitNewPath
  in joinPath $ (replicate droppedPathComponents "..") ++ splitNewFilename
  -- show (newPath, newFilename)


main :: IO ()
main = do
  args0 <- getArgs
  
  let (options, nonOptions, unrecognized, errors) = getOpt' Permute optDescription args0
  
  {-
  mapM_ putStrLn args0
  
  putStrLn "Options:"
  mapM_ print options
  
  putStrLn "NonOptions:"
  mapM_ putStrLn nonOptions
  -}
  
  -- if any errors or unrecognized options, abort
  case errors of
    [] -> return ()
    _  -> do putStrLn "Command line parsing error:"
             mapM_ putStrLn errors
             exitFailure
  
  case unrecognized of
    [] -> return ()
    _  -> do putStrLn "Error: unrecognized command line options:"
             mapM_ putStrLn unrecognized
             exitFailure
  
  
  let exe = "wine"
  let opts0 = [ "C:\\Program Files\\Microsoft Visual Studio 8\\VC\\bin\\cl.exe" ]
  
  sources <- filterM doesFileExist nonOptions
  
  putStrLn "Sources:"
  mapM_ putStrLn sources
  
  let sourceSet = Set.fromList sources
  let opts00 = filter (\x -> not $ Set.member x sourceSet) nonOptions
  
  putStrLn "opts00:"
  mapM_ putStrLn opts00
  
  -- translate options
  let opts1 = defaultMsvcOptions { clOptions = opts0
                                 }
  let opts2 = foldl (flip translateOption) opts1 options
  
  
  -- if source starts with forward slash
  -- it's unix-style absolute filename
  -- but msvc confuses with option
  -- turn it into a relative path
  {-
  pwd <- getCurrentDirectory
  let sources1 = map (\x -> if isRelative x
                                then x
                                else makeRelative pwd x) sources
  -}
  
  -- does not work, WTF?
  -- sources1 <- mapM (\x -> canonicalizePath x >>= \y -> readProcess "winepath" ["-w", y] "" ) sources
  
  pwd <- getCurrentDirectory
  sources1 <- mapM (\x -> if (head x) == '/'
                            then canonicalizePath x >>= return . realMakeRelative pwd
                            else return x
                   ) sources
  
  
  putStrLn "getCurrentDirectory: "
  putStrLn pwd
  
  putStrLn "Sources1:"
  mapM_ putStrLn sources1
  
  
  -- tack include dirs and nonoptions to the end
  let opts3 = (clOptions opts2) ++ sources1
  
  let defaultCompilerOpts = 
              [ "/IC:/winsdk/include"
              , "/IC:/msvcInc"
              , "/IC:/msPlatInc"
              , "/IC:/miscShit"
              ]
  let defaultLinkerOpts = [ "/link"
                   , "/LIBPATH:C:/winsdk/lib"
                   , "/LIBPATH:C:/msvcLib"
                   , "/LIBPATH:C:/msPlatLib"
                   , "/LIBPATH:C:/miscShit"
                   ]
  
  -- add compiler or linker opts depending on mode
  let opts4 =
       if mode opts2 == Compile
         then opts3 ++ defaultCompilerOpts ++ (map ("/I" ++ ) (includeDirs opts2)) ++ opts00
         else opts3 ++ defaultLinkerOpts ++ opts00
  
  
  -- start the compiler in background
  mapM_ putStrLn opts4
  (_, _, _, handle) <- createProcess (proc exe opts4)
  
  -- generate dependency file
  generateDependecyFile (head sources) opts2
  
  -- wait for compiler to finish
  exitCode <- waitForProcess handle
  
  exitWith exitCode


\end{code}
