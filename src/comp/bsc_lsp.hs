{-# LANGUAGE BangPatterns, CPP, OverloadedStrings, DuplicateRecordFields, LambdaCase, DeriveDataTypeable#-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main_bsc_lsp(main, hmain) where

-- Haskell libs
import Prelude
import System.Environment(getArgs, getProgName)
import System.Process(runInteractiveProcess, waitForProcess, readProcessWithExitCode)
import System.Process(system)
import System.Exit(ExitCode(ExitFailure, ExitSuccess))
import System.FilePath(takeDirectory, takeBaseName )
import System.IO(openFile, IOMode(AppendMode, WriteMode), hFlush, stdout, hPutStr, hPutStrLn, stderr, hGetContents, hClose, hSetBuffering, BufferMode(LineBuffering))
import System.IO(hSetEncoding, utf8)
import System.Directory(removeFile)
import System.IO.Error(ioeGetErrorType)
import System.Posix.Files(fileMode,  unionFileModes, ownerExecuteMode, groupExecuteMode, setFileMode, getFileStatus, fileAccess)
import System.Directory(getDirectoryContents, doesFileExist, getCurrentDirectory)
import System.Time(getClockTime, ClockTime(TOD)) -- XXX: from old-time package
import Data.Char(isSpace, toLower, ord)
import Data.List(intersect, nub, partition, intersperse, sort,
            isPrefixOf, isSuffixOf, unzip5, intercalate)
import Data.Time.Clock.POSIX(getPOSIXTime)
import Data.Maybe(isJust, isNothing)
import Numeric(showOct)

import Control.Monad(when, unless, filterM, liftM, foldM)
import Control.Monad.Except(runExceptT)
import Control.Concurrent(forkIO)
import Control.Concurrent.MVar(newEmptyMVar, putMVar, takeMVar)
import qualified Control.Exception as CE
import qualified Data.Map as M
import qualified Data.Set as S

import ListMap(lookupWithDefault)
import SCC(scc)

-- utility libs
import ParseOp
-- import PFPrint
import Util(headOrErr, fromJustOrErr, joinByFst, quote)
import FileNameUtil(baseName, hasDotSuf, dropSuf, dirName, mangleFileName,
                    mkAName, mkVName, mkVPICName,
                    mkNameWithoutSuffix,
                    mkSoName, mkObjName, mkMakeName,
                    bscSrcSuffix, bseSrcSuffix, binSuffix,
                    hSuffix, cSuffix, cxxSuffix, cppSuffix, ccSuffix,
                    objSuffix, useSuffix,
                    genFileName, createEncodedFullFilePath,
                    getFullFilePath, getRelativeFilePath)
import FileIOUtil(writeFileCatch, readFileMaybe, removeFileCatch,
                  readFilePath)
import TopUtils
import SystemCheck(doSystemCheck)
import BuildSystem
import IOUtil(getEnvDef)

-- compiler libs
--import FStringCompat
import Exceptions(bsCatch)
import Flags(
        Flags(..),
        DumpFlag(..),
        hasDump,
        verbose, extraVerbose, quiet)
import FlagsDecode(
        Decoded(..),
        decodeArgs,
        updateFlags,
        showFlags,
        showFlagsRaw,
        exitWithUsage,
        exitWithHelp,
        exitWithHelpHidden)
import Error(ErrorHandle(..), ErrorState(..), internalError, ErrMsg(..), ExcepWarnErr(..),
             initErrorHandle, setErrorHandleFlags,
             bsError, extractPosition, extractMessage, bsWarning, bsMessage,
             exitFail, exitOK, exitFailWith)
import Position(noPosition, cmdPosition, Position (pos_line, pos_column))
import CVPrint
import Id
import Backend
import Pragma
import VModInfo(VPathInfo, VPort)
import Deriving(derive)
import SymTab
import MakeSymTab(mkSymTab, cConvInst)
import TypeCheck(cCtxReduceIO, cTypeCheck)
import PoisonUtils(mkPoisonedCDefn)
import GenSign(genUserSign, genEverythingSign)
import Simplify(simplify)
import ISyntax(IPackage(..), IModule(..),
               IEFace(..), IDef(..), IExpr(..), fdVars)
import ISyntaxUtil(iMkRealBool, iMkLitSize, iMkString{-, itSplit -}, isTrue)
import InstNodes(getIStateLocs, flattenInstTree)
import IConv(iConvPackage, iConvDef)
import FixupDefs(fixupDefs, updDef)
import ISyntaxCheck(tCheckIPackage, tCheckIModule)
import ISimplify(iSimplify)
import BinUtil(BinMap, HashMap, readImports, replaceImports)
import GenBin(genBinFile)
import GenWrap(genWrap, WrapInfo(..))
import GenFuncWrap(genFuncWrap, addFuncWrap)
import GenForeign(genForeign)
import IExpand(iExpand)
import IExpandUtils(HeapData)
import ITransform(iTransform)
import IInline(iInline)
import IInlineFmt(iInlineFmt)
import Params(iParams)
import ASyntax(APackage(..), ASPackage(..),
               ppeAPackage,
               getAPackageFieldInfos)
import ASyntaxUtil(getForeignCallNames)
import ACheck(aMCheck, aSMCheck, aSignalCheck, aSMethCheck)
import AConv(aConv)
import IDropRules(iDropRules)
import ARankMethCalls(aRankMethCalls)
import AState(aState)
import ARenameIO(aRenameIO)
import ASchedule(AScheduleInfo(..), AScheduleErrInfo(..), aSchedule)
import AAddScheduleDefs(aAddScheduleDefs)
import APaths(aPathsPreSched, aPathsPostSched)
import AProofs(aCheckProofs)
import ADropDefs(aDropDefs)
import AOpt(aOpt)
import AVerilog(aVerilog)
import AVeriQuirks(aVeriQuirks)
import VIOProps(VIOProps, getIOProps)
import VFinalCleanup(finalCleanup)
import Synthesize(aSynthesize)
import ABin(ABin(..), ABinModInfo(..), ABinForeignFuncInfo(..),
           ABinModSchedErrInfo(..))
import ABinUtil(readAndCheckABin, readAndCheckABinPathCatch, getABIHierarchy,
                assertNoSchedErr)
import GenABin(genABinFile)
import ForeignFunctions(ForeignFunction(..), ForeignFuncMap,
                        mkImportDeclarations)
import VPIWrappers(genVPIWrappers, genVPIRegistrationArray)
import DPIWrappers(genDPIWrappers)
import SimCCBlock
import SimExpand(simExpand, simCheckPackage)
import SimPackage(SimSystem(..))
import SimPackageOpt(simPackageOpt)
import SimMakeCBlocks(simMakeCBlocks)
import SimCOpt(simCOpt)
import SimBlocksToC(simBlocksToC)
import SystemCWrapper(checkSystemCIfc, wrapSystemC)
import SimFileUtils(analyzeBluesimDependencies)
import Verilog(VProgram(..), vGetMainModName, getVeriInsts)
import Depend
import Version(bscVersionStr, copyright, buildnum)
import Classic
import ILift(iLift)
import ACleanup(aCleanup)
import ATaskSplice(aTaskSplice)
import ADumpSchedule (MethodDumpInfo, aDumpSchedule, aDumpScheduleErr,
                      dumpMethodInfo, dumpMethodBVIInfo)
import ANoInline (aNoInline)
import AAddSchedAssumps(aAddSchedAssumps,aAddCFConditionWires)
import ARemoveAssumps(aRemoveAssumps)
import ADropUndet(aDropUndet)
import SAT(checkSATFlags)
import InlineWires(aInlineWires)
import InlineCReg(aInlineCReg)
import LambdaCalc(convAPackageToLambdaCalc)
import SAL(convAPackageToSAL)

import VVerilogDollar
import ISplitIf(iSplitIf)
import VFileName
import Language.LSP.Server
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP
import Language.LSP.Protocol.Lens qualified as LSP
import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Format
import Language.LSP.VFS qualified as VFS
import Control.Lens ((^.), to) -- Convenient
import Data.Maybe (fromMaybe)
import Data.Aeson qualified as J
import Data.Aeson ( fromJSON )
import GHC.IO(catchException)

import Control.Monad.Reader
import Language.LSP.Protocol.Types (DiagnosticSeverity(DiagnosticSeverity_Error))
import Data.List qualified as L
import qualified Control.Monad as Monad
import Data.Typeable

-- TODO: For the few user-provided variables, maybe the easiest is to simply
-- pass a [bsv_lsp.yaml] file to do all those configuration at the project level
-- (instead of passing it as parameters to the LSP server using RPC)
-- TODO: Investigate if there is already a function for 


-- TODO bsv_exe should be provided by the LSP client
bsc_exe :: FilePath
bsc_exe = "/home/tbourgea/git/bsc-lsp/inst/bin/bsc"

-- TODO bsvUserDir should be provided by the LSP client in the future
bsvUserDir :: [ String ]
bsvUserDir = ["."]
-- bsvUserDir = ["testsuite/lsp"]

pathBsvLibs :: String
pathBsvLibs = Monad.join . L.intersperse ":" $ bsvUserDir ++ ["+"]

-- TODO create the directory if it does not exist otherwise it crashes the LSP server
-- In the future, the build folder should be chosen by the LSP client (as it
-- uses standard BO file, should reuse the project build folder)
buildDir :: String
buildDir = "/tmp/build_bsc"


-- Every options passed to Bluespec except the filename, and [-u] in the case
-- where we compile recursively
commandOptions :: [String]
commandOptions = ["--aggressive-conditions", "-p", pathBsvLibs, "-bdir", buildDir]

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
diagFromExcep(ExcepWarnErr err warn ctxt) = do
    let diag =  map (\x -> let pos = extractPosition x  in
                        LSP.Diagnostic (LSP.mkRange (fromIntegral $ pos_line pos - 1) 0
                                                    (fromIntegral $ pos_line pos - 1) 1000)
                                    (Just DiagnosticSeverity_Error)
                                    Nothing Nothing Nothing
                                    (T.pack (show $ extractMessage x))
                                    Nothing Nothing Nothing ) err
        diagw =  map (\x -> let pos = extractPosition x  in
                        LSP.Diagnostic (LSP.mkRange (fromIntegral $ pos_line pos - 1) 0
                                                    (fromIntegral $ pos_line pos - 1) 1000)
                                    (Just LSP.DiagnosticSeverity_Warning)
                                    Nothing Nothing Nothing
                                    (T.pack $ show $ extractMessage x)
                                    Nothing Nothing Nothing) warn
    (diag, diagw)

emptyDiags :: p -> ([LSP.Diagnostic], [LSP.Diagnostic])
emptyDiags _ = ([],[])


withFilePathFromUri :: MonadLsp config f => LSP.Uri -> (FilePath -> f ()) -> f ()
withFilePathFromUri uri k = 
    case LSP.uriToFilePath uri of
        Just s -> k s
        Nothing -> logForClient . toStrict $ format "URI {} cannot be transformed into Filepath" (Only (show uri))

handlers :: Handlers (LspM ())
handlers =
  mconcat
--   Add handler for completion
    [ notificationHandler LSP.SMethod_Initialized $ \_not -> do
        logForClient "BSC LSP Server Initialized"
    , notificationHandler LSP.SMethod_WorkspaceDidChangeConfiguration $ \_dummy -> do
        -- TODO In the future handle potential dynamic server configuration change here
        return ()
    , notificationHandler LSP.SMethod_TextDocumentDidOpen $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            content = msg ^. LSP.params . LSP.textDocument . LSP.text  -- Extract content first time file is opened
            -- TODO, maybe don't hard fault on Invalid URIs? 
        withFilePathFromUri doc (\file ->  do
            logForClient . toStrict $ format  "Open File {} " (Only file)

            -- First we call the fullblown compiler recursively, to get the [.bo]
            -- files of the submodules and then
            (errCode, stdout', stderr') <- liftIO $ readProcessWithExitCode bsc_exe (commandOptions ++ [ "-u", file ]) ""
        
            -- Independently of the success of the previous step (technically we
            -- could skip it if the previous step was successful) we call the
            -- frontend of the compiler that we carved out in the current file (it
            -- compiles up to typechecking), in case where an error is found
            -- (parsing, importing submodules, typechecking, etc...), an exception
            -- is raised by the compiler.  We catch this exception here with, and we
            -- display it as diagnostic
            -- TODO: Improve the diagnostic display using bsc printing function for errors
            (diagErrs, diagWarns) <- liftIO $ catchException (emptyDiags <$> hmain (commandOptions ++ [file])  (T.unpack content)) $
                                                return . diagFromExcep
            diagsForClient doc $ diagErrs ++ diagWarns)
    , notificationHandler LSP.SMethod_TextDocumentDidChange $ \msg -> do -- VFS automatically contains the content
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
        withFilePathFromUri doc (\file -> do
            mdoc <- getVirtualFile (doc ^. to LSP.toNormalizedUri)
            case mdoc of
                Just vf@(VFS.VirtualFile _ version rope) -> do
                    (diagErrs, diagWarns) <- liftIO $ catchException (emptyDiags <$> hmain (commandOptions ++ [file])  (T.unpack (VFS.virtualFileText vf))) $
                                                return . diagFromExcep
                    diagsForClient doc (diagErrs ++ diagWarns)
                Nothing -> errorForClient . toStrict $ format "No virtual file found for {} " (Only file))
    , notificationHandler LSP.SMethod_TextDocumentDidSave $ \msg -> do -- Check what is being written
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
        withFilePathFromUri doc (\file -> do
            mdoc <- getVirtualFile (doc ^. to LSP.toNormalizedUri)
            case mdoc of
              Just vf@(VFS.VirtualFile _ version _rope) -> do
                let stdin' = ""
                (errCode, stdout', stderr') <- liftIO $ readProcessWithExitCode bsc_exe (commandOptions ++ [file]) stdin'
                -- Delete old BO file to if we saved a new broken file 
                when (errCode /= ExitSuccess) . liftIO $ removeFile (buildDir ++ takeBaseName file ++ ".bo")
                (diagErrs, diagWarns) <- liftIO $ catchException (emptyDiags <$> hmain (commandOptions ++ [file])  (T.unpack (VFS.virtualFileText vf))) $
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

    -- TODO: Investigate if the notion of documentation is understood internally by the BSC compiler
    -- , requestHandler LSP.SMethod_TextDocumentDocumentSymbol$ \req responder -> do

    -- -- TODO: At some point, the following seemed to be necessary? 
    -- , notificationHandler LSP.SMethod_SetTrace $ \msg -> do
    --     return ()
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
        ServerDefinition { parseConfig = \_old new -> case fromJSON new of
            J.Success v -> Right v
            J.Error err -> Left $ T.pack err
            --  const $ const $ Right ()
                         , onConfigChange = const $ pure ()
                         , defaultConfig = ()
                         , configSection = "demo"
                         , doInitialize = \env _req ->
                            -- liftIO $ putStrLn "Initialize?"
                            pure $ Right env
                         , staticHandlers = \_caps -> handlers
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
hmain args contentFile = do
    pprog <- getProgName
    cdir <- getEnvDef "BLUESPECDIR" dfltBluespecDir
    bscopts <- getEnvDef "BSC_OPTIONS" ""
    let args' = words bscopts ++ args
    -- reconstruct original command line (modulo whitespace)
    -- add a newline at the end so it is offset
    let cmdLine = concat ("Invoking command line:\n" : (intersperse " " (pprog:args'))) ++ "\n"
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
    flags' <- checkSATFlags errh flags

    -- check system requirements
    -- doSystemCheck errh
    let comp = compile_no_deps
    comp errh flags' name contentFile


compile_no_deps :: ErrorHandle -> Flags -> String -> String -> IO (Bool)
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
    -- handle <- openFile "/tmp/outputLSP.txt" WriteMode 
    -- hPutStr handle name_orig
    -- hPutStr handle file
    -- hClose handle
    (pkg@(CPackage i _ _ _ _ _), t)
        <- parseSrc False errh flags True name file
    -- [T]: In the case where there is a parsing error, what to do?

    let dumpnames = (baseName (dropSuf name), getIdString (unQualId i), "")
    -- Now we can go to TC, but we need to do a few passes of compilation before that
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
    clkTime <- getClockTime
    epochTime <- getPOSIXTime

    -- Values needed for the Environment module
    let env =
            [("compilerVersion",iMkString $ bscVersionStr True),
             ("date",                iMkString $ show clkTime),
             ("epochTime",      iMkLitSize 32 $ floor epochTime),
             ("buildVersion",   iMkLitSize 32 $ buildnum),
             ("genPackageName", iMkString $ getIdBaseString pkgId),
             ("testAssert",        iMkRealBool $ testAssert flags)
            ]

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
    -- [T]: Here we have the tyupechecking errors we can return them to LSP client
    -- mod is actually never used!
    --putStr (ppReadable mod)
    t <- dump errh flags t DFtypecheck dumpnames mod
    -- [T]: We should not use this function to compile the bo of thje submodule so here we are good.

    return ( not tcErrors, binmap, hashmap)


-- Our server should compile subpackets

