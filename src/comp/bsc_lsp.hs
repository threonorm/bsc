{-# LANGUAGE CPP, OverloadedStrings, DuplicateRecordFields, DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
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
      getCurrentDirectory )
import Data.Maybe(isJust)

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
--import FStringCompat
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
import Position(Position (..))
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
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Format ( format, Only(Only) )
import Language.LSP.VFS qualified as VFS
import Control.Lens ((^.), to) -- Convenient
import Data.Aeson qualified as J
import GHC.IO(catchException)

import Data.List qualified as L
import Control.Monad qualified as Monad
import PPrint qualified as P
import GHC.Generics (Generic)

-- TODO: For the project-level variables (pathBsvLibs, bscExtraArgs), maybe the easiest is to simply
-- pass a [bsv_lsp.yaml] file to do all those configuration at the project level

data Config = Config {bscExe :: FilePath, buildDir:: FilePath}
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
commandOptions :: Config -> [String]
commandOptions cfg = ["--aggressive-conditions"] ++ bscExtraArgs ++ ["-p", pathBsvLibs cfg, "-bdir", buildDir cfg]

logForClient :: MonadLsp config f => T.Text -> f ()
logForClient x = sendNotification LSP.SMethod_WindowLogMessage $ LSP.LogMessageParams LSP.MessageType_Log x

-- TODO: I did not check how MessageType_Error appear on the vscode side
errorForClient :: MonadLsp config f => T.Text -> f ()
errorForClient x = sendNotification LSP.SMethod_WindowLogMessage $ LSP.LogMessageParams LSP.MessageType_Error x

diagsForClient :: MonadLsp config f => LSP.Uri -> [LSP.Diagnostic] -> f ()
diagsForClient doc diags = sendNotification LSP.SMethod_TextDocumentPublishDiagnostics $ LSP.PublishDiagnosticsParams doc Nothing diags

-- We modified the error handling of the compiler to generate an exception [ExcepWarnErr] on warnings and errors
-- This function transform such an exception into LSP diagnostics
diagFromExcep  :: ExcepWarnErr -> ([LSP.Diagnostic], [LSP.Diagnostic])
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
    (diag, diagw)

emptyDiags :: p -> ([LSP.Diagnostic], [LSP.Diagnostic])
emptyDiags _ = ([],[])

withFilePathFromUri :: MonadLsp config f => LSP.Uri -> (FilePath -> f ()) -> f ()
withFilePathFromUri uri k =
    case LSP.uriToFilePath uri of
        Just s -> k s
        Nothing -> logForClient . toStrict $ format "URI {} cannot be transformed into Filepath" (Only (show uri))

handlers :: Handlers (LspM Config)
handlers =
  mconcat
--   Add handler for completion
    [ notificationHandler LSP.SMethod_Initialized $ \_not -> do
        cfg <- getConfig 
        logForClient . toStrict $ format "BSC LSP Server Initialized {} " (Only (show cfg))
    , notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_dummy -> do
        -- See Note about LSP configuration in the haskell lsp package
        return ()
    , notificationHandler LSP.SMethod_TextDocumentDidOpen $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            content = msg ^. LSP.params . LSP.textDocument . LSP.text  -- Extract content first time file is opened
        cfg <- getConfig
        withFilePathFromUri doc (\file -> do
            -- logForClient . toStrict $ format  "Open File {} " (Only file)

            -- First we call the fullblown compiler recursively, to get the [.bo]
            -- files of the submodules and then
            (errCode, stdout', stderr') <- liftIO $ readProcessWithExitCode (bscExe cfg) (commandOptions cfg ++ [ "-u", file ]) ""

            -- Independently of the success of the previous step (technically we
            -- could skip it if the previous step was successful) we call the
            -- frontend of the compiler in the current file (it compiles up to
            -- typechecking), in case where an error is found (parsing,
            -- importing submodules, typechecking, etc...), an exception is
            -- raised by the compiler.  We catch this exception here  and we
            -- display it as diagnostic
            (diagErrs, diagWarns) <- liftIO $ catchException (emptyDiags <$> hmain (commandOptions cfg ++ [file])  (T.unpack content)) $
                                                return . diagFromExcep
            diagsForClient doc $ diagErrs ++ diagWarns
            )
    , notificationHandler LSP.SMethod_TextDocumentDidChange $ \msg -> do -- VFS automatically contains the content
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
        withFilePathFromUri doc (\file -> do
            mdoc <- getVirtualFile (doc ^. to LSP.toNormalizedUri)
            cfg <- getConfig
            case mdoc of
                Just vf@(VFS.VirtualFile _ version rope) -> do
                    (diagErrs, diagWarns) <- liftIO $ catchException (emptyDiags <$> hmain (commandOptions cfg ++ [file])  (T.unpack (VFS.virtualFileText vf))) $
                                                return . diagFromExcep
                    diagsForClient doc (diagErrs ++ diagWarns)
                Nothing -> errorForClient . toStrict $ format "No virtual file found for {} " (Only file))
    , notificationHandler LSP.SMethod_TextDocumentDidSave $ \msg -> do -- Check what is being written
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
        withFilePathFromUri doc (\file -> do
            mdoc <- getVirtualFile (doc ^. to LSP.toNormalizedUri)
            cfg <- getConfig
            case mdoc of
              Just vf@(VFS.VirtualFile _ version _rope) -> do
                (errCode, stdout', stderr') <- liftIO $ readProcessWithExitCode (bscExe cfg) (commandOptions cfg ++ [file]) ""
                -- Delete stale BO file as the new source is broken 
                when (errCode /= ExitSuccess) . liftIO $ removeFile (buildDir cfg ++ takeBaseName file ++ ".bo")
                (diagErrs, diagWarns) <- liftIO $ catchException (emptyDiags <$> hmain (commandOptions cfg ++ [file])  (T.unpack (VFS.virtualFileText vf))) $
                                                return . diagFromExcep
                diagsForClient doc (diagErrs ++ diagWarns)
              Nothing -> errorForClient . toStrict $ format "No virtual file found for {} " (Only file))
    -- , requestHandler LSP.SMethod_TextDocumentDefinition $ \req responder -> do
    --     let pos = req ^. LSP.params . LSP.position -- pos is a position in teh document
    --         uri = req ^. LSP.params . LSP.textDocument . LSP.uri -- uri is the link of the document
    --     -- TODO
    --     -- Make a state monad that contains the compiled modules so far and their public identifier
    --     -- For local identifiers, also query the VFS to get the current file, to know what identifier is at the position pos.
    --     -- for that we will probably need to index what BSC produced, to build a table for the current file.
    --     -- We will want to build the indexed table (Identifier <-> [positions])
    --     -- (Note: Could be several definitions?)
    --     defs <- undefined
    --     responder $ undefined

    -- -- TODO: Investigate if the notion of documentation is understood internally by the BSC compiler
    -- , requestHandler LSP.SMethod_TextDocumentDocumentSymbol$ \req responder -> do

    -- -- TODO: At some point, the following seemed to be necessary? 
    , notificationHandler LSP.SMethod_SetTrace $ \msg -> do
        return ()
    ]


main :: IO Int
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    handle <- liftIO $ openFile "/tmp/output.txt" WriteMode
    liftIO $ hPutStr handle "Starting LSP\n"
    liftIO $ hClose handle
    runServer $
        ServerDefinition { -- TODO Parse config create a dependency on AESON and is currently not really useful (as config is ())
                           -- Still we keep it as a placeholder
                           parseConfig = \_old new -> case J.fromJSON new of
                                                            J.Success v -> Right v
                                                            J.Error err -> Left $ T.pack err
                         , onConfigChange = const $ pure ()
                         , defaultConfig = Config {buildDir = buildDirDefault, bscExe = bscExeDefault }
                         , configSection = "glscp.bsclsp" -- TODO investigate what this configSection is suppose to do
                         , doInitialize = \env _req -> pure $ Right env
                         , staticHandlers = const handlers
                         , interpretHandler = \env -> Iso (runLspT env) liftIO
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
hmain :: [String] -> String -> IO Bool
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


main' :: ErrorHandle -> Flags -> String -> String -> IO Bool
main' errh flags name contentFile =  do
    setErrorHandleFlags errh flags
    tStart <- getNow

    -- check system and SAT solver requirements, LSP uses BSC as an executable so we assume BSC will be unhappy 
    -- if the system is invalid for BSC, no need to redo those system checks all the time
    -- flags' <- checkSATFlags errh flags
    -- doSystemCheck errh
    let comp = compile_no_deps
    comp errh flags name contentFile


compile_no_deps :: ErrorHandle -> Flags -> String -> String -> IO Bool
compile_no_deps errh flags name contentFile = do
  (ok, _, _) <- compileFile errh flags M.empty M.empty name contentFile -- Pass the string that contains the thing to tc
  return ok

-- returns whether the compile errored or not
compileFile :: ErrorHandle -> Flags -> BinMap HeapData -> HashMap -> String -> String ->
               IO (Bool, BinMap HeapData, HashMap)
compileFile errh flags binmap hashmap name_orig file = do

    handle <- openFile "/tmp/output.txt" AppendMode
    hPutStr handle "Start Compiling\n"
    hClose handle
    pwd <- getCurrentDirectory
    let name = (createEncodedFullFilePath name_orig pwd)

    let syntax = (if      hasDotSuf bscSrcSuffix name then CLASSIC
                  else if hasDotSuf bseSrcSuffix name then ESE
                  else BSV)
    setSyntax syntax

    t <- getNow
    -- ===== the break point between file manipulation and compilation
    (pkg@(CPackage i _ _ _ _ _), t)
        <- parseSrc False errh flags True name file

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
    IO (Bool, BinMap HeapData, HashMap)
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
    return ( not tcErrors, binmap, hashmap)


-- Our server should compile subpackets

