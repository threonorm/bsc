{-# LANGUAGE BangPatterns, CPP, OverloadedStrings, DuplicateRecordFields, LambdaCase #-}
module Main_bsc_lsp(main, hmain) where

-- Haskell libs
import Prelude
import System.Environment(getArgs, getProgName)
import System.Process(runInteractiveProcess, waitForProcess)
import System.Process(system)
import System.Exit(ExitCode(ExitFailure, ExitSuccess))
import System.FilePath(takeDirectory)
import System.IO(hFlush, stdout, hPutStr, stderr, hGetContents, hClose, hSetBuffering, BufferMode(LineBuffering))
import System.IO(hSetEncoding, utf8)
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
import Error(internalError, ErrMsg(..),
             ErrorHandle, initErrorHandle, setErrorHandleFlags,
             bsError, bsWarning, bsMessage,
             exitFail, exitOK, exitFailWith)
import Position(noPosition, cmdPosition)
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
import Language.LSP.VFS qualified as VFS
import Control.Lens ((^.), to) -- Convenient
import Data.Maybe (fromMaybe)


-- bsc_tc should update tc the content, it should also store the results, indexed by URI + version 
-- What to do for the dependencies that are recompiled on-the-fly? Do we have an URI for them?
bsc_tc = undefined
-- args <- getArgs
-- bsCatch (hmain args)
handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler LSP.SMethod_Initialized $ \_not -> do
        -- Maybe we should compile everything here?
        return ()
    , notificationHandler LSP.SMethod_TextDocumentDidOpen $ \msg -> do
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            content = msg ^. LSP.params . LSP.textDocument . LSP.text  -- Extract content first time file is opened
        -- Call "bsc -u filename", do we actually have the filename here, from doc?
        -- We might need some yaml configuration file to know the paths toward the dependencies (if they are not local)
        -- Then call main : hmain ["--aggressive-conditions", (LSP.toNormalizedUri doc)] content
        -- per URI we should record the latest usable CPackage
        bsc_tc (LSP.toNormalizedUri doc) Nothing content
    , notificationHandler LSP.SMethod_TextDocumentDidChange $ \msg -> do -- VFS automatically contains the content
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri . to LSP.toNormalizedUri
        mdoc <- getVirtualFile doc
        case mdoc of
          Just vf@(VFS.VirtualFile _ version _rope) -> do
            -- Then call main : hmain ["--aggressive-conditions", (LSP.toNormalizedUri doc)] content
            -- per URI we should record the latest usable CPackage
            bsc_tc doc (Just $ fromIntegral version) (VFS.virtualFileText vf)
          _ -> undefined -- error
    , notificationHandler LSP.SMethod_TextDocumentDidSave $ \msg -> do -- Check what is being written
        let doc = msg ^. LSP.params . LSP.textDocument . LSP.uri
            content = fromMaybe "?" $ msg ^. LSP.params . LSP.text
            -- call main : hmain ["--aggressive-conditions", (LSP.toNormalizedUri doc)] content
            -- then call bsc -u filename
            -- per URI we should record the latest usable CPackage
        bsc_tc (LSP.toNormalizedUri doc) Nothing content
    , requestHandler LSP.SMethod_TextDocumentDefinition $ \req responder -> do
        let pos = req ^. LSP.params . LSP.position -- pos is a position in teh document
            uri = req ^. LSP.params . LSP.textDocument . LSP.uri -- uri is the link of the document
        -- Make a state monad that contains the compiled modules so far, query this big table 
        -- Query all the compiled modules that are in scope and nonqualified, and ask for global symbols
        -- Also query the VFS to get the current file, to know what identifier is at the position pos.
        -- for that we will probably need to index what BSC produced, to build a table for the current file.
        -- We will want ot build the indexed table (Identifier <-> [positions])
        -- (Note: Could be several definitions?)
        defs <- undefined
        responder $ undefined
    ]
     

main :: IO Int
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    runServer $
        ServerDefinition { parseConfig = const $ const $ Right ()
                         , onConfigChange = const $ pure ()
                         , defaultConfig = ()
                         , configSection = "demo"
                         , doInitialize = \env _req -> pure $ Right env
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
    -- bsc can raise exception,  catch them here  print the message and exit out.


-- Use with hugs top level
hmain :: [String] -> String -> IO ()
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
    errh <- initErrorHandle
    let doWarnings = when ((not . null) warnings) $ bsWarning errh warnings
        setFlags = setErrorHandleFlags errh
    case decoded of
        DHelp flags ->
            do { setFlags flags; doWarnings;
                 exitWithHelp errh pprog args' cdir }
        DHelpHidden flags ->
            do { setFlags flags; doWarnings;
                 exitWithHelpHidden errh pprog args' cdir }
        DUsage -> exitWithUsage errh pprog
        DError msgs -> bsError errh msgs
        DNoSrc flags ->
            -- XXX we want to allow "bsc -v" and similar calls
            -- XXX to print version, etc, but if there are other flags
            -- XXX on the command-line we probably should report an error
            do { setFlags flags; doWarnings; showPreamble flags;
                 exitOK errh }
        DBlueSrc flags src ->
            do { setFlags flags; doWarnings; showPreamble flags;
                 main' errh flags src contentFile;
                 exitOK errh }
       

main' :: ErrorHandle -> Flags -> String -> String -> IO ()
main' errh flags name contentFile =  do
    setErrorHandleFlags errh flags
    tStart <- getNow
    flags' <- checkSATFlags errh flags

    -- check system requirements
    -- doSystemCheck errh
    let comp = compile_no_deps
    success <- comp errh flags' name contentFile

    -- final verbose message
    if success then
      return ()
     else
       exitFail errh

compile_no_deps :: ErrorHandle -> Flags -> String -> String -> IO (Bool)
compile_no_deps errh flags name contentFile = do
  (ok, _, _) <- compileFile errh flags M.empty M.empty name contentFile -- Pass the string that contains the thing to tc
  return ok

-- returns whether the compile errored or not
compileFile :: ErrorHandle -> Flags -> BinMap HeapData -> HashMap -> String -> String ->
               IO (Bool, BinMap HeapData, HashMap)
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

