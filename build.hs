
import Data.List
import Data.Char
import Data.Maybe
import Data.Ord
import Data.Function
import qualified Data.List.Split as Split
import Control.Monad
import Prelude hiding (readFile, writeFile, putStrLn, print)
import System.IO hiding (readFile, writeFile, putStrLn, hGetLine, hGetContents, print)
import System.IO.UTF8
import System.Directory
import System.FilePath ((</>))
import System.Environment (getArgs)
import System.Process (runInteractiveCommand, waitForProcess)
import System.Exit (ExitCode(..))
import Control.Exception

--this can be modified to create shortcuts to some files
--the file MUST be in the <package name>/src/ folder!
packageToModule = [("jshaskell", "jshaskell/src/Haskell.js")
                  ,("prelude"  , "base/src/Prelude.js")
                  ,("pretty"   , "pretty/src/Text/PrettyPrint/HughesPJ.js")
                  ,("jsparsec" , "jsparsec/src/Text/Parsec.js")
                  ,("webbits"  , "WebBits/src/BrownPLT/JavaScript.js")
                  ,("example"  , "example/src/parsejs.js")
                  ]

{-
parse refernces :: [Ref] -> 
resolve dependencies, and create file list (+ closures and local imports) :: [Task] -> 
reorganize files, remove some closures and multiple local imports :: [Modules] ->
create local variables for local imports by executing build.js ++ all the actual JS (__temp.js) ->
replace evals with actual local variable imports -> write single JS file + a minified one

TODO: 
 * option for closure compiler
 * local var collision (+check equality, skip operators)

-}

main = do
    (path':args) <- getArgs
    let path     = fromMaybe path' $ lookup (map toLower path') packageToModule
    let fileName = takeWhile (/='/') path ++ "-" ++ replace "_" "." (qualifiedName path) ++ ".js"
    let tempFile = "__temp.js"

    putStrLn$   "\nResolving dependencies for " ++ nl ++ show path

    tasks       <- createFileList path
    let mods    = (simplifyModules . toModules . reverse) tasks
    --mapM_ (putStrLn . (" * " ++) . show ) $ map (\m -> (map qualifiedName $ mLocals m, map qualifiedName $ mIncludes m)) $ mods
    mapM_ putStrLn $ flip map mods $ ("\n * " ++) . replace "_" "." . intercalate ",\n   " . map qualifiedName . mIncludes

    putStrLn    "\nComputing localized imports\n"

    v8tasks     <- mapM toV8Task mods
    buildjs     <- readFile "build.js"
    let v8tasks' = buildjs : concat v8tasks ++ [";_print_( _locals_.join('|||'));"]
    writeFile   tempFile $ concat v8tasks'
    (locals', exitCode) <- runCmd $ "d8 " ++ tempFile ++ " -- " ++ genModuleNamesToBeLocalized mods
    unless      (exitCode == ExitSuccess) $ error locals'
    removeFile  tempFile

    putStrLn$   "Creating standalone js file: " ++ nl ++ show fileName

    let locals  = filter (/= "|||") (Split.split (Split.onSublist "|||") locals' )
    let final   = init . trim $ insertLocals v8tasks' locals
    let pkgNames = getPackageNames mods
    licenses    <- getLicenses pkgNames
    writeFile   fileName $ intercalate (nl ++ nl) (reverse licenses) ++ wrap (concat final)
    
    putStrLn    "\nRunning Closure compiler"

    compile     fileName >>= putStrLn . ("Closure compiler " ++)
    
    putStrLn ""


--represents the intellisense reference declarations
--with an optional "local" attribute, which will load the referenced module to the current scope
--otherwise only the "qualified" path can be used (e.g. NS.Data_Function.on)
data Ref = Ref {
    modulePath  :: FilePath,
    refPath     :: FilePath,
	local       :: Bool
} deriving (Show, Read)

--after the references are resolved, they are transformed into a list of tasks
data Task = Include FilePath
          | Localize FilePath
          | ClosureStart
          | ClosureEnd
    deriving (Eq, Ord, Show)
 
--then the tasks are transformed to a list of Modules, which are more abstract
data Module = Module {
    mPath       :: FilePath,
    mIncludes   :: [FilePath],
    mLocals     :: [FilePath]
} deriving (Eq, Ord, Show)




toModules :: [Task] -> [Module]
toModules [] = []
toModules tasks@(ClosureStart:rest) = 
    Module path includes' locals' : toModules (drop (1 + length inclAndLoc) rest)
  where
    inclAndLoc = takeWhile (/= ClosureEnd) rest
    (includes, locals) = flip partition inclAndLoc $ \x -> case x of {Include _ -> True; _ -> False}
    path       = goUp $ case head includes of Include p -> p
    includes'  = flip map includes $ \(Include p) -> p
    locals'    = flip map locals $ \(Localize p) -> p


simplifyModules :: [Module] -> [Module]
simplifyModules mods = mergeModules $ sortModules mods where

    mergeTwoModules a b = Module (mPath a) (nub $ mIncludes a ++ mIncludes b) (nub $ mLocals a ++ mLocals b)

    mergeModules mods = concatMap (foldl fold []) $ groupBy (eq `on` mPath) (mods) where
        fold [] x = [x]
        fold acc@(prev:rest) x = mergeTwoModules prev x : rest

    sortModules mods = sortBy (comparing enum) mods where
        order = nubBy (eq `on` mPath) (mods)
        enum mod = head $ findIndices (\x -> mPath mod == mPath x) order


--this is used intead of $ show [moduleNames]
--will be sent as a single argument to v8, so it can't contain spaces and doublequotes
genModuleNamesToBeLocalized :: [Module] -> String
genModuleNamesToBeLocalized mods = 
    replace "\"" "'" $ replace " " "" $ show list
  where
    list = mapMaybe f mods
    f (Module _ _ []) = Nothing
    f (Module _ _ localsRefs) = Just $ concat $intersperse "','" $ map qualifiedName localsRefs

localJSString = ";var _ns_ = _createNS_(_moduleStrArray_.shift());_locals_.push(_ns_);eval(_ns_);"

qualifiedName :: FilePath -> String
qualifiedName path = (init.init.init) $ replace "/" "_" $ Split.split (Split.onSublist "/src/") path !! 2


toV8Task :: Module -> IO [String]
toV8Task (Module _ includes locals) =  do
        let localStrs = replicate (length locals) localJSString
        cntnts  <- mapM readFile includes
        return  $ (";(function(){" ++ nl) : localStrs ++ cntnts ++ [nl ++ "}());"]

insertLocals :: [String] -> [String] -> [String]
insertLocals v8tasks locals = map fst $ foldr (\ (jsstring, i) acc@((_,j):_) -> 
            if jsstring == localJSString 
                then (locals !! j, j - 1) : acc
                else (jsstring, j) : acc
        ) [("", length locals - 1)] (zip v8tasks [0, 0 ..])


getPackageNames :: [Module] -> [String]
getPackageNames mods = nub $ map (takeWhile (/='/') . mPath) mods

getLicenses :: [FilePath] -> IO [String]
getLicenses pkgNames = forM pkgNames $ \pkgName -> do
    let fileName = pkgName </> "license.txt"
    fileExists <- doesFileExist fileName
    if fileExists
      then readFile fileName
      else return []


createFileList :: FilePath -> IO [Task]
createFileList path = fold [] (Include path) where 
    fold :: [Task] -> Task -> IO [Task]
    fold acc task@(Include path) = 
      if task `elem` acc then return acc else do
        refs   <- getRefs path
        deps   <- foldM fold acc (map (Include . refPath) refs)
        let locals = catMaybes $ flip map refs $ \ref -> if local ref then Just $ Localize (refPath ref) else Nothing
        return $ ClosureEnd : task : locals ++ [ClosureStart] ++ deps
    fold acc _ = return acc


--reads references from a single file
getRefs :: FilePath -> IO [Ref]
getRefs path = do
    inh  <- openFile path ReadMode
    refs <- readRefs inh []
    hClose inh
    return $ reverse refs
  where
    readRefs :: Handle -> [Ref] -> IO [Ref]
    readRefs inh acc = do
        iseof <- hIsEOF inh
        if iseof
            then return acc
            else do line' <- hGetLine inh
                    let line = replace "\65279" "" line'
                    if isRef line
                        then readRefs inh $ parseRef line : acc
                        else return acc

    isRef line = "///<referencepath" `isPrefixOf` filter (/= ' ') line

    parseRef line = Ref { modulePath = path
                        , refPath = getAbsPath path $ takeWhile (/= '"') $ tail $ dropWhile (/= '"') line
                        , local   = "local" `isInfixOf` line
                        }

goUp :: FilePath -> FilePath
goUp path = reverse $ dropWhile (/='/') $ if "/" `isSuffixOf` path 
                                            then tail $ reverse path
                                            else reverse path

--Refs contain the absolute path of the file they were used for, 
--with which the refernces absoulute path can be computed
getAbsPath :: FilePath -> FilePath -> FilePath
getAbsPath modulePath refPath = 
      if not $ "../" `isPrefixOf` refPath
                    then curDir ++ refPath
                    else up refPath curDir
    where 
        curDir    = reverse $ dropWhile (/='/') $ reverse modulePath
        up refPath curDir = 
            if not $ "../" `isPrefixOf` refPath
                then curDir ++ refPath
                else up (drop 3 refPath) (goUp curDir)

instance Eq Ref where
    r1 == r2 = refPath r1 == refPath r2


compile :: FilePath -> IO String
compile path = 
    do  (_, exitCode)  <- run
        return $ if exitCode == ExitSuccess then "succeeded" else "failed"
    where
        run = runCmd $ "java -jar closure-compiler/compiler.jar --js " ++ path ++ " --js_output_file " ++ minPath ++ ".js"
        minPath = take (length path - 3) path ++ "-min"

runCmd :: String -> IO (String, ExitCode)
runCmd command =  do 
    (_, hOutput, _, hProcess) <- runInteractiveCommand command
    sOutput  <- hGetContents hOutput
    exitCode <- foldr seq (waitForProcess hProcess) sOutput
    return   (sOutput, exitCode)



showModule :: FilePath -> String
showModule path = (init. init . init) $ replace "/" "." $ (trimPathLeft . trimPathLeft) path
    where
        trimPathLeft path = tail $ dropWhile (/='/') path


eq a b = a == b

trim = tail . init

wrap str = ";(function(){" ++ nl ++ str ++ nl ++ "}());"

nl = "\r\n"

--from Data.List.Utils
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = join new . split old $ l where
    split _ [] = []
    split delim str = let (firstline, remainder) = breakList (isPrefixOf delim) str in
        firstline : case remainder of {[] -> [];
            x  -> if x == delim then [[]] else split delim (drop (length delim) x) }
    join delim l = concat (intersperse delim l)
    breakList func = spanList (not . func)
    spanList _ [] = ([],[])
    spanList f list@(x:xs) = if f list then (x:ys,zs) else ([],list) where (ys,zs) = spanList f xs
