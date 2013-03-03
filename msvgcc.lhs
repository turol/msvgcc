#!/usr/bin/env runhaskell

\begin{code}
{- OPTIONS_GHC  -Wall -Werror -}


{-
  Originally written by Turo Lamminen.
  This code is in the Public Domain and comes with ABSOLUTELY NO WARRANTY.
  If it breaks you get to keep all the pieces.
-}


module Main (main) where
import Control.Monad (filterM)
import Data.Char (isSpace, toLower)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, sort)
import qualified Data.Set as Set(
                         fromList
                       , member
                       )
import System.Console.GetOpt
import System.Directory (doesFileExist
                       , canonicalizePath
                       , getCurrentDirectory
                       )
import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.IO (Handle, hGetLine, hIsEOF)
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
translateOption DependencyGenerate     oldOpts = (addOpt "/showIncludes" oldOpts) { generateDeps = True }
translateOption DependencyPhonyHeaders oldOpts = oldOpts { depPhonyHeaders = True }
translateOption (DependencyTarget f)   oldOpts = oldOpts { depTarget = f }
translateOption (IncludeDir dir)       oldOpts = oldOpts { includeDirs = (snoc (includeDirs oldOpts) dir) }
translateOption (OptFlag _)            oldOpts = oldOpts  -- FIXME: do something interesting with this


-- msvc has different option for object and exe output name...
translateOption (OutputFile out) oldOpts =
  addOpt (newOpt ++ out) (oldOpts { depTarget = out, depFile = reverse $ "d." ++ (drop 4 $ reverse out) })
  where
    newOpt =
      case mode oldOpts of
        Compile        -> "/Fo"
        CompileAndLink -> "/Fe"

-- translateOption str@_ _ = error $ "fuck " ++ (show str)
-- translateOption _ oldOpts = oldOpts


replaceBackslash :: Char -> Char
replaceBackslash '\\' = '/'
replaceBackslash c@_  = c


isInDefaultIncludeDir :: String -> Bool
isInDefaultIncludeDir filename =
  any (\dir -> isPrefixOf (map toLower dir) (map toLower filename)) defaultIncludeDirs


fromWineFiles :: [String] -> IO [String]
fromWineFiles [] = return []
fromWineFiles files = do
  out <- readProcess "winepath" files ""
  
  let lines0 = lines $ filter (/= '\r') out
  out2 <- case lines0 of
            [] -> return []
            _  -> readProcess "realpath" lines0 ""
  
  return $ lines out2


-- read msvc output, grab include files to dependency file
generateDependencies :: MsvcOptions -> ProcessHandle -> Handle -> IO ExitCode
generateDependencies opts msvcProcess outHandle = do
  (exitCode, includes') <- readOutputLoop []
  
  let targetName = depTarget opts
  let file = depFile opts
  
  -- drop prefix, change '\' to '/', sort and drop duplicates
  let includes'' = nub $ sort $ map (map replaceBackslash . dropWhile isSpace . (drop (length "Note: including file:"))) includes'
  
  -- drop headers in default include dirs
  let includes''' = filter (not . isInDefaultIncludeDir) includes''
  
  includes <- fromWineFiles includes'''
  
  let deps =
       -- the main target
       targetName ++ ": " ++ (intercalate " " includes)
        ++ "\n"
         -- empty target for all include files
         -- to avoid build errors when include files deleted
        ++ (concatMap (\x -> x ++ ":\n") includes)
  
  writeFile file deps
  return exitCode
  where
    
  	-- read input until process ends
    -- put includes in a list
    -- print everything else in stdout
    readOutputLoop :: [String] -> IO (ExitCode, [String])
    readOutputLoop accum = do
      eof <- hIsEOF outHandle
      if eof
        then do c <- waitForProcess msvcProcess
                return (c, accum)
        else do 
                l <- hGetLine outHandle
                let isInclude = isInfixOf "Note: including file" l
                let newAccum = if isInclude
                                 then l:accum
                                 else accum
                
                -- not include, put in stdout
                if not isInclude
                  then putStrLn l
                  else return ()
                
                readOutputLoop newAccum


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


defaultIncludeDirs :: [String]
defaultIncludeDirs =
 [ "C:/winsdk/include"
 , "C:/msvcInc"
 , "C:/msPlatInc"
 , "C:/miscShit"
 ]


defaultLibDirs :: [String]
defaultLibDirs = 
  [ "c:/winsdk/lib"
  , "c:/msvcLib"
  , "c:/msPlatLib"
  , "c:/miscShit"
  ]


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
  
  sources0 <- filterM doesFileExist nonOptions
  
  let sourceSet = Set.fromList sources0
  let opts00 = filter (\x -> not $ Set.member x sourceSet) nonOptions
  
  let sources = filter (\x -> not (".d" `isSuffixOf` x)) sources0
  putStrLn "Sources:"
  mapM_ putStrLn sources
  
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
  
  let defaultCompilerOpts = map (\x -> "/I" ++ x) defaultIncludeDirs
  let defaultLinkerOpts = "/link":(map (\x -> "/LIBPATH:" ++ x) defaultLibDirs)
  
  -- add compiler or linker opts depending on mode
  let opts4 =
       if mode opts2 == Compile
         then opts3 ++ defaultCompilerOpts ++ (map ("/I" ++ ) (includeDirs opts2)) ++ opts00
         else opts3 ++ defaultLinkerOpts ++ opts00
  
  
  -- start the compiler in background
  mapM_ putStrLn opts4
  let outHandle = if generateDeps opts2
                    then CreatePipe
                    else Inherit
  let msvcProcess = (proc exe opts4) { std_out = outHandle }
  (_, msvcOut, _, handle) <- createProcess msvcProcess
  
  -- wait for compiler to finish
  -- generate dependency file
  exitCode <- case msvcOut of
                Nothing   -> waitForProcess handle
                Just o    -> generateDependencies opts2 handle o
  
  exitWith exitCode


\end{code}
