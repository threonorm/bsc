{-# LANGUAGE CPP, OverloadedStrings, DuplicateRecordFields, DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use mapMaybe" #-}
module Main_bsc_lsp(main, hmain) where

-- Haskell libs
import Prelude
import System.Environment(getProgName)
import System.Process (readProcessWithExitCode)
import System.Exit(ExitCode(ExitSuccess))
import System.FilePath(takeBaseName)
import System.IO
    ( openFile,
      IOMode(AppendMode, WriteMode),
      stdout,
      hPutStr,
      stderr,
      hClose,
      hSetBuffering,
      BufferMode(LineBuffering),
      hSetEncoding,
      utf8 )
import System.Directory
    ( removeFile,
      getCurrentDirectory,
      createDirectoryIfMissing )
import Data.Maybe(isJust, catMaybes, fromJust, fromMaybe)

import PreStrings(fsEmpty)
import Control.Monad(when)
import qualified Data.Map as M


-- utility libs
import ParseOp
-- import PFPrint
import FileNameUtil(baseName, hasDotSuf, dropSuf,
                    bscSrcSuffix, bseSrcSuffix,
                    createEncodedFullFilePath)
import TopUtils
import IOUtil(getEnvDef)

-- compiler libs
import FStringCompat
import Flags(
        Flags(..),
        DumpFlag(..),
        verbose)
import FlagsDecode(
        Decoded(..),
        decodeArgs,
        showFlags,
        showFlagsRaw)
import Error(ErrorHandle, ExcepWarnErr(..),
             prEMsg, swarning, serror,
             initErrorHandle, setErrorHandleFlags,
             extractPosition,  bsWarning)
import Position(Position (..),
        getPositionLine,
        getPositionFile,
        getPositionColumn,
        mkPosition)
import CVPrint
import Id
import Deriving(derive)
import MakeSymTab(mkSymTab, cConvInst)
import TypeCheck(cCtxReduceIO, cTypeCheck)
import BinUtil(BinMap, HashMap, readImports)
import GenWrap(genWrap)
import GenFuncWrap(genFuncWrap, addFuncWrap)
import IExpandUtils(HeapData)

import Depend
import Version(bscVersionStr, copyright)
import Classic


import Language.LSP.Server
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Protocol.Lens qualified as LSP
-- import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans (lift, liftIO)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Format ( format, Only(Only) )
import Language.LSP.VFS qualified as VFS
import Control.Lens ((^.), to) -- Convenient
import Data.Aeson qualified as J
import GHC.IO(catchException)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, putMVar, modifyMVar_)

import Data.List qualified as L
import Control.Monad qualified as Monad
import PPrint qualified as P
import GHC.Generics (Generic)
import CSyntax (extractStructTDelf, extractStructTDef)
import qualified PFPrint as Pp
import Data.Aeson.Types (parse)

-- Current limitations: 
--  1. Errors are at the line level (no multiline errors, no
--  end of column error).
--  2. bsc_lsp is singlethreaded
--  3. No support for project yaml file
--  4. TODO: Return the package and the name table as best as possible even if compilation fail (currently returns nothing)
--  5. TODO: Currently points to the type definition for methods, instead of the module instantiation. 
--     Maybe that should be the behavior for getting the types, but for the definition it should be
--     The actual definition?
--  6. How to get local definitions?
--  7. Go to [instance.subinterface1.subsub.method]  in general this is very
--     difficult. Already instance.method() is tricky as we would need to know
--     the line where method is defined -> Maybe special case it as it would be
--     quite useful? -> Call the frontend for the subfile to find the position
--     of the method


-- Strawman to get some number of local definitions manually
-- TODO get more, right now we only gather some local module instances and
-- module parameters We don't go down in rules as it is painful to find

isSameIdBefore :: Id -> Id -> Bool
isSameIdBefore current target = 
    current == target && getPositionLine (getIdPosition current) <= getPositionLine (getIdPosition target)


getPreviousPosFromParsedPackage :: Id -> CPackage -> [Id]
getPreviousPosFromParsedPackage pos (CPackage _ _ _ _ defns _) = 
    -- TODO limitation currently will potentially return unrelated definitions in previous
    -- modules of the package, we should only keep the defns which contains the pos
    -- TODO to make that easier it would be good to write a function
    -- that returns the (minpos, maxpos) in an arbitrary piece of CSyntax
    concatMap (pruneLocalCDefn pos) defns

pruneLocalCDefn :: Id -> CDefn  -> [Id]
pruneLocalCDefn pos cdefn =
    case cdefn of
        CValueSign cdef ->
             pruneLocalCDef pos  cdef 
        _ -> [] 
      

pruneLocalCDef :: Id -> CDef -> [Id]
pruneLocalCDef pos cdef =
    case cdef of
        CDef id cqtype cclauses -> 
            --  let new = if isSameIdBefore id pos then [id] else [] in 
            --  new ++ 
            concatMap (pruneLocalCClause pos) cclauses
        _ -> [] 

pruneLocalCClause :: Id -> CClause -> [ Id ]
pruneLocalCClause pos cdefn =
    case cdefn of 
        CClause args _ c -> 
            getIdEqualArgs pos args ++ pruneLocalCExpr pos c

getIdEqualArgs :: Id -> [CPat] -> [Id]
getIdEqualArgs pos [] = []
getIdEqualArgs pos ((CPVar x):tl) = ( if x `isSameIdBefore` pos then [x] else [] ) ++ (getIdEqualArgs pos tl)
getIdEqualArgs pos (_:tl) = (getIdEqualArgs pos tl)


pruneLocalCMStmt :: Id -> CMStmt -> [Id]
pruneLocalCMStmt pos stmt = 
    case stmt of 
        CMStmt s -> pruneLocalCStmt pos s 
        _ -> []


-- Minimum syndical - try to find just the definitions of instances in modules
pruneLocalCStmt :: Id -> CStmt -> [Id]
pruneLocalCStmt pos stmt = 
    case stmt of 
        CSBindT (CPVar x) _ _ t rhs -> if x `isSameIdBefore` pos then [x] else []
        CSBind (CPVar x) _ _ rhs -> if x `isSameIdBefore` pos then [x] else []
        CSletseq [CLValue x cclauses _] -> if x `isSameIdBefore` pos then [x] else []
        _ -> []



pruneLocalCExpr :: Id -> CExpr -> [Id]
pruneLocalCExpr pos cexpr  =  
    case cexpr of 
        Cmodule _ cmstmts -> concatMap (pruneLocalCMStmt pos) cmstmts 
        _ -> []



-- TODO EASY: For the project-level variables (pathBsvLibs, bscExtraArgs), maybe the easiest is to simply
-- pass a [bsv_lsp.yaml] file to do all those configuration at the project level

data Config = Config {bscExe :: FilePath}
  deriving (Generic, J.ToJSON, J.FromJSON, Show)

-- TODO bsv_exe should be provided by the LSP client
bscExeDefault :: FilePath
bscExeDefault = "bsc"

-- TODO bsvUserDir should be provided by the LSP client in the future
bsvUserDir :: [String]
bsvUserDir = []

pathBsvLibs :: Config -> String
pathBsvLibs cfg = Monad.join . L.intersperse ":" $ bsvUserDir ++ ["+"]

bscExtraArgs :: [String]
bscExtraArgs = []

-- TODO create the directory if it does not exist otherwise it crashes the LSP server
-- In the future, the build folder should be chosen by the LSP client.
-- As it uses standard BO file, it *might* make sense to reuse the project build
-- folder, more thought are needed.
buildDirDefault :: FilePath
buildDirDefault = "/tmp/build_bsc/"


-- Every options passed to Bluespec except the filename, and [-u] in the case
-- where we compile recursively
commandOptions :: Config -> String -> [String]
commandOptions cfg builddir = ["--aggressive-conditions"] ++ bscExtraArgs ++ ["-p", pathBsvLibs cfg, "-bdir", builddir]

logForClient :: MonadLsp config f => T.Text -> f ()
logForClient x = sendNotification LSP.SMethod_WindowLogMessage $ LSP.LogMessageParams LSP.MessageType_Log x

-- TODO: I did not check how MessageType_Error appear on the vscode side
errorForClient :: MonadLsp config f => T.Text -> f ()
errorForClient x = sendNotification LSP.SMethod_WindowLogMessage $ LSP.LogMessageParams LSP.MessageType_Error x

diagsForClient :: MonadLsp config f => LSP.Uri -> [LSP.Diagnostic] -> f ()
diagsForClient doc diags = sendNotification LSP.SMethod_TextDocumentPublishDiagnostics $ LSP.PublishDiagnosticsParams doc Nothing diags

-- We modified the error handling of the compiler to generate an exception [ExcepWarnErr] on warnings and errors
-- This function transform such an exception into LSP diagnostics
diagFromExcep  :: ExcepWarnErr -> ([LSP.Diagnostic], [LSP.Diagnostic], Maybe a, Maybe b)
diagFromExcep (ExcepWarnErr err warn ctxt) = do
    let diag =  map (\x -> let pos = extractPosition x  in
                        LSP.Diagnostic (LSP.mkRange (fromIntegral $ pos_line pos - 1) 0
                                                    (fromIntegral $ pos_line pos - 1) 1000)
                                    (Just LSP.DiagnosticSeverity_Error)
                                    Nothing Nothing Nothing
                                    (T.pack . P.pretty 78 78 . prEMsg serror ctxt $ x)
                                    -- TODO remove following lines
                                    -- (T.pack (show $ extractMessage x))
                                    Nothing Nothing Nothing ) err
        diagw =  map (\x -> let pos = extractPosition x  in
                        LSP.Diagnostic (LSP.mkRange (fromIntegral $ pos_line pos - 1) 0
                                                    (fromIntegral $ pos_line pos - 1) 1000)
                                    (Just LSP.DiagnosticSeverity_Warning)
                                    Nothing Nothing Nothing
                                    (T.pack . P.pretty 78 78 . prEMsg swarning ctxt $ x)
                                    -- (T.pack $ show $ extractMessage x)
                                    Nothing Nothing Nothing) warn
    (diag, diagw, Nothing, Nothing)

emptyDiags :: (Bool, c, p, p) -> ([LSP.Diagnostic], [LSP.Diagnostic], Maybe c, Maybe p)
emptyDiags (_,map, packagetc, package) = ([],[], Just map, Just package)

withFilePathFromUri :: MonadLsp config f => LSP.Uri -> (FilePath -> f ()) -> f ()
withFilePathFromUri uri k =
    case LSP.uriToFilePath uri of
        Just s -> k s
        Nothing -> logForClient . toStrict $ format "URI {} cannot be transformed into Filepath" (Only (show uri))

-- getIdStruct :: -> [Id]
getIdStructs :: CDefn -> [Id]
getIdStructs x = concat $ case x of
     CValueSign l ->  (\(CStructT t v) -> fst <$> v) <$> extractStructTDef l
     _ -> []

updateNametable :: LSP.Uri -> Maybe (BinMap HeapData) -> Maybe CPackage-> LspState ()
updateNametable doc maybepackage cpackage =
    -- This function sssumes that if [maybepackage] is Nothing, then [cpackage]
    -- is also Nothing (Both datastructure should be Nothing simultaneously)
    Monad.forM_ cpackage (\c -> 
        Monad.forM_ maybepackage (\m -> do
                        stRef <- lift ask
                        let defs_public_aux = L.map (\(x,y,z,t,u) -> y) $ M.elems m
                            defs_public = L.concatMap (\(CSignature _namePackage _imported _fixity defs) ->
                                                        L.concatMap
                                                            -- (\x -> definedNames x ++ getIdStructs x)
                                                            definedNames
                                                            defs) defs_public_aux
                            fulldefs_local = (\(CPackage _ _ _ _ defns _) -> L.concatMap definedNames defns) c
                            all_defs = fulldefs_local ++ defs_public -- Might contain duplicate
                        -- logForClient . T.pack $  L.concatMap (\(CPackage _ _ _ _ defns _) -> Pp.pp80 defns) cpackage
                        liftIO . modifyMVar_ stRef $ \x ->
                                                    return $ x{visible_global_identifiers = M.insert
                                                                    doc all_defs (visible_global_identifiers x),
                                                                parsedByUri = M.insert doc c (parsedByUri x)}))

type LspState = LspT Config (ReaderT (MVar ServerState) IO)

data ServerState = ServerState {
    -- Currently easy implementation for global identifiers, 
    -- To avoid ambiguities of goto definitions because different
    -- packages could import packages that define the same identifier
    -- we keep one Id position table per Uri defined

    -- TODO: This loose sharing when a module is imported twice,  see if we can
    -- maintain a single map Id -> Position
    -- TODO: UI question - what to do when file does not compile, should we 
    -- keep the old mapping or delete it? 
    -- Currently leaning toward keeping the old mapping.
    visible_global_identifiers :: M.Map LSP.Uri [Id],
    typeId :: M.Map LSP.Uri (M.Map Id Id),
    parsedByUri :: M.Map LSP.Uri CPackage,
    buildDir :: FilePath
    -- We are missing the definitions here

    -- In the future, this type should carry:
    -- TODO Add documentations 
    -- TODO Add Uri -> NonGlobalDefinitions even for broken files
    -- TODO Add all references to Id? Id -> [(Uri, Position)]
    -- TODO Type definitions vs value definitions.
    -- Constructor are type definitions
}

idAtPos :: LSP.Position -> T.Text -> Maybe (Int, (Int, Int), T.Text)
idAtPos pos text =
    -- the id is necessarily on a single line first extract the line
    let line_n :: Int = fromIntegral $ pos ^. LSP.line
        column_n :: Int = fromIntegral $ pos ^. LSP.character
        line = T.lines text L.!! line_n in
    (\(x,y,z) -> (line_n, (x,y), z)) <$> extractWordAroundPosition column_n line

extractWordAroundPosition :: Int -> T.Text -> Maybe (Int, Int, T.Text)
extractWordAroundPosition position text = case findWordBoundaries position text of
    Just (start, end) -> Just (start, end, T.take (end - start) (T.drop start text))
    Nothing           -> Nothing

findWordBoundaries :: Int -> T.Text -> Maybe (Int, Int)
findWordBoundaries position text = do
    let (before, after) = T.splitAt position text
    let start = T.length (T.takeWhile isWordChar (T.reverse before))
    let end = T.length (T.takeWhile isWordChar after) + position
    return (position - start, end)

isWordChar :: Char -> Bool
isWordChar c = c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9']

bscPosToLSPPos :: (Int, (Int,Int)) -> Position -> LSP.DefinitionLink
bscPosToLSPPos (l ,(start,end)) pos =
    let line_target =  fromIntegral $ getPositionLine pos - 1
        range = LSP.Range (LSP.Position line_target 0) (LSP.Position line_target 1000) in
    LSP.DefinitionLink $ LSP.LocationLink
        (Just . LSP.Range (LSP.Position (fromIntegral l) (fromIntegral start)) $ LSP.Position (fromIntegral l) (fromIntegral end))
        (LSP.filePathToUri $ getPositionFile pos)
        range
        range --(LSP.filePathToUri $ getPositionFile pos) _

getWorkspace :: LspState FilePath
getWorkspace = do
            stRef <- lift ask
            servState <- liftIO $ readMVar stRef
            return $ buildDir servState

-- handlers :: Handlers (LspM Config)
handlers :: Handlers LspState
handlers =
  mconcat
--   Add handler for completion
    [ notificationHandler LSP.SMethod_Initialized $ \_not -> do
        cfg <- getConfig
        root <- getRootPath
        case root of
         Just root -> do
            let workspace = root ++ "/.bsclsp"
            liftIO $ createDirectoryIfMissing True workspace
            stRef <- lift ask
            liftIO . modifyMVar_ stRef $ \x -> return $ x{buildDir= workspace}
            logForClient . toStrict $ format "BSC LSP Server Initialized {} workspace {} " (show cfg, root)
         Nothing -> do
            let workspace = "/tmp/.globalbsclsp"
            liftIO $ createDirectoryIfMissing True workspace
            stRef <- lift ask
            liftIO . modifyMVar_ stRef $ \x -> return $ x{buildDir= workspace}
    , notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_dummy -> do
        -- See Note about LSP configuration in the haskell lsp package
        return ()
    , notificationHandler LSP.SMethod_TextDocumentDidOpen $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            content = msg ^. LSP.params . LSP.textDocument . LSP.text  -- Extract content first time file is opened
        cfg <- getConfig
        workspace <- getWorkspace
        withFilePathFromUri doc (\file -> do
            -- logForClient . toStrict $ format  "Open File {} " (Only file)

            -- First we call the fullblown compiler recursively, to get the [.bo]
            -- files of the submodules and then
            (errCode, stdout', stderr') <- liftIO $ readProcessWithExitCode (bscExe cfg) (commandOptions cfg workspace ++ [ "-u", file ]) ""

            -- Independently of the success of the previous step (technically we
            -- could skip it if the previous step was successful) we call the
            -- frontend of the compiler in the current file (it compiles up to
            -- typechecking), in case where an error is found (parsing,
            -- importing submodules, typechecking, etc...), an exception is
            -- raised by the compiler.  We catch this exception here  and we
            -- display it as diagnostic
            (diagErrs, diagWarns, maybeNameTable, maybePackage) <- liftIO $ catchException (emptyDiags <$> hmain (commandOptions cfg workspace ++ [file])  (T.unpack content)) $
                                                return . diagFromExcep
            diagsForClient doc $ diagErrs ++ diagWarns
            updateNametable doc maybeNameTable maybePackage)
    , notificationHandler LSP.SMethod_TextDocumentDidChange $ \msg -> do -- VFS automatically contains the content
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
        workspace <- getWorkspace
        withFilePathFromUri doc (\file -> do
            mdoc <- getVirtualFile (doc ^. to LSP.toNormalizedUri)
            cfg <- getConfig
            case mdoc of
                Just vf@(VFS.VirtualFile _ version rope) -> do
                    (diagErrs, diagWarns, maybeNameTable, maybePackage) <- liftIO $ catchException (emptyDiags <$> hmain (commandOptions cfg workspace ++ [file])  (T.unpack (VFS.virtualFileText vf))) $
                                                return . diagFromExcep
                    diagsForClient doc (diagErrs ++ diagWarns)
                    updateNametable doc maybeNameTable maybePackage
                Nothing -> errorForClient . toStrict $ format "No virtual file found for {} " (Only file))
    , notificationHandler LSP.SMethod_TextDocumentDidSave $ \msg -> do -- Check what is being written
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
        workspace <- getWorkspace
        withFilePathFromUri doc (\file -> do
            mdoc <- getVirtualFile (doc ^. to LSP.toNormalizedUri)
            cfg <- getConfig
            case mdoc of
              Just vf@(VFS.VirtualFile _ version _rope) -> do
                (errCode, stdout', stderr') <- liftIO $ readProcessWithExitCode (bscExe cfg) (commandOptions cfg workspace ++ [file]) ""
                -- Delete stale BO file as the new source is broken 
                when (errCode /= ExitSuccess) . liftIO $ removeFile (workspace ++ takeBaseName file ++ ".bo")
                (diagErrs, diagWarns, maybeNameTable, maybePackage) <- liftIO $ catchException (emptyDiags <$> hmain (commandOptions cfg workspace ++ [file])  (T.unpack (VFS.virtualFileText vf))) $
                                                return . diagFromExcep
                diagsForClient doc (diagErrs ++ diagWarns)
                updateNametable doc maybeNameTable maybePackage
              Nothing -> errorForClient . toStrict $ format "No virtual file found for {} " (Only file))
    , requestHandler LSP.SMethod_TextDocumentDefinition $ \req responder -> do
        let pos = req ^. LSP.params . LSP.position -- pos is a position in the document
            doc = req ^. LSP.params . LSP.textDocument . LSP.uri -- uri is the link of the document
        withFilePathFromUri doc (\file -> do
            mdoc <- getVirtualFile (doc ^. to LSP.toNormalizedUri)
            cfg <- getConfig
            case mdoc of
              Just vf@(VFS.VirtualFile _ version _rope) -> do
                    stRef <- lift ask
                    serverState <- liftIO $ readMVar stRef
                    case M.lookup doc $ visible_global_identifiers serverState of
                        Just ids -> do
                            let add_key = (\id -> (getIdBase id, id)) <$> ids
                                searched = idAtPos pos (VFS.virtualFileText vf)
                                (line, (colbegin,colend), idname) = fromMaybe (-1, (-1,-1), "_unused_42") $ searched
                                find_ids_glob = snd <$> L.filter (\(key,v) -> toString key == T.unpack idname) add_key
                                find_ids_local = getPreviousPosFromParsedPackage (mkId (mkPosition fsEmpty line colbegin ) .  fromString $ T.unpack idname) . 
                                                    fromMaybe (CPackage undefined undefined undefined undefined [] undefined) $ M.lookup doc $ parsedByUri serverState
                                find_ids = find_ids_local ++ find_ids_glob 
                            responder . Right $ LSP.InR $ LSP.InL $ bscPosToLSPPos ((\(l,(cs,ce),z)-> (l,(cs,ce))) $ fromJust searched) <$> getIdPosition  <$> find_ids
                        Nothing ->
                            responder . Right $ LSP.InR $ LSP.InL []
              Nothing -> errorForClient . toStrict $ format "No virtual file found for {} " (Only file))
    , requestHandler LSP.SMethod_TextDocumentSignatureHelp $ \req responder -> do
        let pos = req ^. LSP.params . LSP.position -- pos is a position in the document
            doc = req ^. LSP.params . LSP.textDocument . LSP.uri -- uri is the link of the document
        withFilePathFromUri doc (\file -> do
            mdoc <- getVirtualFile (doc ^. to LSP.toNormalizedUri)
            cfg <- getConfig
            case mdoc of
              Just vf@(VFS.VirtualFile _ version _rope) -> do
                    stRef <- lift ask
                    serverState <- liftIO $ readMVar stRef
                    return ()
              Nothing -> errorForClient . toStrict $ format "No virtual file found for {} " (Only file))

    -- -- TODO: Investigate if the notion of documentation is understood internally by the BSC compiler
    -- , requestHandler LSP.SMethod_TextDocumentDocumentSymbol$ \req responder -> do

    -- -- TODO: Don't know what SetTrace is used for, but vscode keeps sending
    -- those event, so we make a dummy handler.
    , notificationHandler LSP.SMethod_TextDocumentDidClose $ \msg -> do -- Check what is being written
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
        -- TODO: free the data corresponding to the file?
        return ()
    , notificationHandler LSP.SMethod_SetTrace $ \msg -> do
        return ()
    ]



initialServerState :: IO (MVar ServerState)
initialServerState = do
    newMVar ServerState{ visible_global_identifiers = M.empty, buildDir = "/tmp/.globalbsclsp", typeId = M.empty, parsedByUri = M.empty }

runLSPAndState :: LspState a  -> MVar ServerState -> LanguageContextEnv Config ->  IO a
runLSPAndState lsp state env = runReaderT (runLspT env lsp) state

main :: IO Int
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    state <- initialServerState
    runServer $
        ServerDefinition { parseConfig = \_old new -> case J.fromJSON new of
                                                            J.Success v -> Right v
                                                            J.Error err -> Left $ T.pack err
                         , onConfigChange = const $ pure ()
                         , defaultConfig = Config {bscExe = bscExeDefault }
                         , configSection = "glspc.initializationOptions" -- TODO investigate what this configSection is suppose to do
                         , doInitialize = \env _req -> pure $ Right env
                         , staticHandlers = const handlers
                        --  , interpretHandler = \env -> Iso (runLspT env) liftIO
                         , interpretHandler = \env -> Iso (\lsp -> runLSPAndState lsp state env) liftIO
                         , options = defaultOptions { -- set sync options to get DidSave event, as well as Open and Close events.
                                             optTextDocumentSync =
                                               Just
                                                 ( LSP.TextDocumentSyncOptions
                                                     (Just True)
                                                     (Just LSP.TextDocumentSyncKind_Full) -- Easier for now, could try to play with incremental?
                                                     (Just False)
                                                     (Just False)
                                                     (Just . LSP.InR . LSP.SaveOptions $ Just True)
                                                 ) }
                         }


-- Use with hugs top level
-- hmain :: [String] -> String -> IO Bool
hmain :: [String] -> String -> IO (Bool, BinMap HeapData, CPackage, CPackage)
hmain args contentFile = do
    pprog <- getProgName
    cdir <- getEnvDef "BLUESPECDIR" dfltBluespecDir
    bscopts <- getEnvDef "BSC_OPTIONS" ""
    let args' = words bscopts ++ args
    -- reconstruct original command line (modulo whitespace)
    -- add a newline at the end so it is offset
    let cmdLine = concat ("Invoking command line:\n" : (L.intersperse " " (pprog:args'))) ++ "\n"
    let showPreamble flags = do
          when (verbose flags) $ putStrLnF (bscVersionStr True)
          when (verbose flags) $ putStrLnF copyright
          when ((verbose flags) || (printFlags flags)) $ putStrLnF cmdLine
          when ((printFlags flags) || (printFlagsHidden flags)) $
                putStrLnF (showFlags flags)
          when (printFlagsRaw flags) $ putStrLnF (showFlagsRaw flags)
    let (warnings, decoded) = decodeArgs (baseName pprog) args' cdir
    errh <- initErrorHandle True
    let doWarnings = when ((not . null) warnings) $ bsWarning errh warnings
        setFlags = setErrorHandleFlags errh
    case decoded of
        DBlueSrc flags src ->
            do { setFlags flags; doWarnings; showPreamble flags;
                 main' errh flags src contentFile
                }
        _ -> error "Internal error bsc_lsp"


-- main' :: ErrorHandle -> Flags -> String -> String -> IO Bool
main' :: ErrorHandle -> Flags -> String -> String -> IO (Bool, BinMap HeapData, CPackage, CPackage)
main' errh flags name contentFile =  do
    setErrorHandleFlags errh flags
    tStart <- getNow

    -- check system and SAT solver requirements, LSP uses BSC as an executable so we assume BSC will be unhappy 
    -- if the system is invalid for BSC, no need to redo those system checks all the time
    -- flags' <- checkSATFlags errh flags
    -- doSystemCheck errh
    let comp = compile_no_deps
    comp errh flags name contentFile


-- compile_no_deps :: ErrorHandle -> Flags -> String -> String -> IO Bool
compile_no_deps :: ErrorHandle -> Flags -> String -> String -> IO (Bool, BinMap HeapData, CPackage, CPackage)
compile_no_deps errh flags name contentFile = do
  (ok, loaded, _, cpackage, parsed) <- compileFile errh flags M.empty M.empty name contentFile -- Pass the string that contains the thing to tc
  return (ok, loaded, cpackage, parsed)

-- returns whether the compile errored or not
compileFile :: ErrorHandle -> Flags -> BinMap HeapData -> HashMap -> String -> String ->
               IO (Bool, BinMap HeapData, HashMap, CPackage, CPackage)
compileFile errh flags binmap hashmap name_orig file = do

    pwd <- getCurrentDirectory
    let name = (createEncodedFullFilePath name_orig pwd)

    let syntax = (if      hasDotSuf bscSrcSuffix name then CLASSIC
                  else if hasDotSuf bseSrcSuffix name then ESE
                  else BSV)
    setSyntax syntax

    t <- getNow
    -- ===== the break point between file manipulation and compilation
    (pkg@(CPackage i _ _ _ _ _), t)
        <- parseSrc (syntax == CLASSIC) errh flags True name file

    let dumpnames = (baseName (dropSuf name), getIdString (unQualId i), "")
    compilePackage errh flags dumpnames t binmap hashmap name pkg

-------------------------------------------------------------------------

compilePackage ::
    ErrorHandle ->
    Flags ->
    DumpNames ->
    TimeInfo ->
    BinMap HeapData ->
    HashMap ->
    String ->
    CPackage ->
    IO (Bool, BinMap HeapData, HashMap, CPackage, CPackage)
compilePackage
    errh
    flags                -- user switches
    dumpnames
    tStart
    binmap0
    hashmap0
    name -- String --
    min@(CPackage pkgId _ _ _ _ _) = do

    handle <- openFile "/tmp/output.txt" AppendMode
    liftIO $ hPutStr handle "Start Package\n"
    liftIO $ hClose handle

    -- Values needed for the Environment module
    -- TODO: This was dead code in this mutilated compiler, suspicious
    -- let env =
    --         [("compilerVersion",iMkString $ bscVersionStr True),
    --          ("date",                iMkString $ show clkTime),
    --          ("epochTime",      iMkLitSize 32 $ floor epochTime),
    --          ("buildVersion",   iMkLitSize 32 $ buildnum),
    --          ("genPackageName", iMkString $ getIdBaseString pkgId),
    --          ("testAssert",        iMkRealBool $ testAssert flags)
    --         ]

    start flags DFimports
    -- Read imported signatures
    (mimp@(CPackage _ _ imps _ _ _), binmap, hashmap)
        <- readImports errh flags binmap0 hashmap0 min

    -- [T]: binmap contains all the submodules that we care about
    -- Can we know which local identifiers point to what?

    t <- dump errh flags tStart DFimports dumpnames mimp

    start flags DFopparse
    mop <- parseOps errh mimp
    t <- dump errh flags t DFopparse dumpnames mop

    -- Generate a global symbol table
    --
    -- Later stages will introduce new symbols that will need to be added
    -- to the symbol table.  Rather than worry about properly inserting
    -- the new symbols, we just build the symbol table fresh each time.
    -- So this is the first of several times that the table is built.
    -- We can't delay the building of the table until after all symbols
    -- are known, because these next stages need a table of the current
    -- symbols.
    --
    start flags DFsyminitial
    symt00 <- mkSymTab errh mop
    t <- dump errh flags t DFsyminitial dumpnames symt00

    -- whether we are doing code generation for modules
    let generating = isJust (backend flags)

    -- Turn `noinline' into module definitions
    start flags DFgenfuncwrap
    (mfwrp, symt0, funcs) <- genFuncWrap errh flags generating mop symt00
    t <- dump errh flags t DFgenfuncwrap dumpnames mfwrp

    -- Generate wrapper for Verilog interface.
    start flags DFgenwrap
    (mwrp, gens) <- genWrap errh flags (genName flags) generating mfwrp symt0
    t <- dump errh flags t DFgenwrap dumpnames mwrp

    -- Rebuild the symbol table because GenWrap added new types
    -- and typeclass instances for those types
    start flags DFsympostgenwrap
    symt1 <- mkSymTab errh mwrp
    t <- dump errh flags t DFsympostgenwrap dumpnames symt1

    -- Re-add function definitions for `noinline'
    mfawrp <- addFuncWrap errh symt1 funcs mwrp

    -- Turn deriving into instance declarations
    start flags DFderiving
    mder <- derive errh flags symt1 mfawrp
    t <- dump errh flags t DFderiving dumpnames mder

    -- Rebuild the symbol table because Deriving added new instances
    start flags DFsympostderiving
    symt11 <- mkSymTab errh mder
    t <- dump errh flags t DFsympostderiving dumpnames symt11

    -- Reduce the contexts as far as possible
    start flags DFctxreduce
    mctx <- cCtxReduceIO errh flags symt11 mder
    t <- dump errh flags t DFctxreduce dumpnames mctx

    -- Rebuild the symbol table because CtxReduce has possibly changed
    -- the types of top-level definitions
    start flags DFsympostctxreduce
    symt <- mkSymTab errh mctx
    t <- dump errh flags t DFsympostctxreduce dumpnames symt

    -- Turn instance declarations into ordinary definitions
    start flags DFconvinst
    let minst = cConvInst errh symt mctx
    t <- dump errh flags t DFconvinst dumpnames minst

    -- Type check and insert dictionaries
    start flags DFtypecheck
    (mod, tcErrors) <- cTypeCheck errh flags symt minst
    -- TODO [T]: Here we have the tyupechecking errors we can return them to LSP client
    -- mod is actually never used! We should probably return mod here, for identifying stuff like

    --putStr (ppReadable mod)
    t <- dump errh flags t DFtypecheck dumpnames mod
    return ( not tcErrors, binmap, hashmap, mod, min)


-- Our server should compile subpackets

