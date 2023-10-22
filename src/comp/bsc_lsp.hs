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
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types qualified as J
import Control.Monad.IO.Class
import Data.Text qualified as T


-- args <- getArgs
-- bsCatch (hmain args)
handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler J.SInitialized $ \_not -> do
        -- Maybe we should compile everything here?
        return ()
    , notificationHandler J.STextDocumentDidOpen $ \msg -> do
        let doc = msg ^. J.params . J.textDocument . J.uri
            content = msg ^. J.params . J.textDocument . J.text  -- Extract content first time file is opened
        validateSwarmCode (J.toNormalizedUri doc) Nothing content
    , notificationHandler J.STextDocumentDidChange $ \msg -> do -- GetContent from VFS
        let doc = msg ^. J.params . J.textDocument . J.uri . to J.toNormalizedUri
        mdoc <- getVirtualFile doc
        case mdoc of
          Just vf@(VirtualFile _ version _rope) -> do
            validateSwarmCode doc (Just $ fromIntegral version) (virtualFileText vf)
          _ -> debug $ "No virtual file found for: " <> from (show msg)
    , notificationHandler J.STextDocumentDidSave $ \msg -> do -- Check what is being written
        let doc = msg ^. J.params . J.textDocument . J.uri
            content = fromMaybe "?" $ msg ^. J.params . J.text
        validateSwarmCode (J.toNormalizedUri doc) Nothing content
    , requestHandler J.STextDocumentDefinition $ \req responder -> do
        let pos = req ^. J.params . J.position -- pos is a position in teh document
            uri = req ^. J.params . J.textDocument . J.uri -- uri is the link of the document
        normUri <- normalizeUriWithPath uri
        store <- getStore
        defs <- runMaybeT $ do
            lift $ debugM $ "Looking up " <> J.getUri (J.fromNormalizedUri normUri) <> " in " <> T.pack (show (M.keys $ I.idxModules store))
            entry <- I.getModule normUri
            lift $ fetchDefinitions store entry pos
        responder $ Right $ J.InR $ J.InR $ J.List $ fromMaybe [] defs
    ]
        
    --      notificationHandler SMethod_Initialized $ \_not -> do
    --     let params =
    --           ShowMessageRequestParams
    --             MessageType_Info
    --             "Turn on code lenses?"
    --             (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
    --     _ <- sendRequest SMethod_WindowShowMessageRequest params $ \case
    --       Right (InL (MessageActionItem "Turn on")) -> do
    --         let regOpts = CodeLensRegistrationOptions (InR Null) Nothing (Just False)

    --         _ <- registerCapability SMethod_TextDocumentCodeLens regOpts $ \_req responder -> do
    --           let cmd = Command "Say hello" "lsp-hello-command" Nothing
    --               rsp = [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
    --           responder $ Right $ InL rsp
    --         pure ()
    --       Right _ ->
    --         sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Info "Not turning on code lenses")
    --       Left err ->
    --         sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error $ "Something went wrong!\n" <> T.pack (show err))
    --     pure ()
    -- , requestHandler SMethod_TextDocumentHover $ \req responder -> do
    --     let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
    --         Position _l _c' = pos
    --         rsp = Hover (InL ms) (Just range)
    --         ms = mkMarkdown "Hello world"
    --         range = Range pos pos
    --     responder (Right $ InL rsp)
    -- ]

--import Debug.Trace

main :: IO ()
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
                                             textDocumentSync =
                                               Just
                                                 ( J.TextDocumentSyncOptions
                                                     (Just True)
                                                     (Just syncKind)
                                                     (Just False)
                                                     (Just False)
                                                     (Just . J.InR . J.SaveOptions $ Just True)
                                                 ) }
                         }
    -- bsc can raise exception,  catch them here  print the message and exit out.


-- Use with hugs top level
hmain :: [String] -> IO ()
hmain args = do
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
                 main' errh flags src;
                 exitOK errh }
       


main' :: ErrorHandle -> Flags -> String -> IO ()
main' errh flags name =  do
    tStart <- getNow

    flags' <- checkSATFlags errh flags

    -- check system requirements
    doSystemCheck errh

    let comp = if updCheck flags'
               then compile_with_deps
               else compile_no_deps

    success <- comp errh flags' name

    -- final verbose message
    _ <- timestampStr flags' "total" tStart

    if success then
      return ()
     else
       exitFail errh

compile_with_deps :: ErrorHandle -> Flags -> String -> IO (Bool)
compile_with_deps errh flags name = do
    let
        verb = showUpds flags && not (quiet flags)
        -- the flags to "compileFile" when re-compiling depended modules
        flags_depend = flags { updCheck = False,
                               genName = [],
                               showCodeGen = verb }
        -- the flags to "compileFile" when re-compiling this module
        flags_this = flags_depend { genName = genName flags }
        comp (success, binmap0, hashmap0) fn = do
            when (verb) $ putStrLnF ("compiling " ++ fn)
            let fl = if (fn == name)
                     then flags_this
                     else flags_depend
            (cur_success, binmap, hashmap)
                <- compileFile errh fl binmap0 hashmap0 fn
            return (cur_success && success, binmap, hashmap)
    when (verb) $ putStrLnF "checking package dependencies"

    t <- getNow
    let dumpnames = (baseName (dropSuf name), "", "")

    -- get the list of depended files which need recompiling
    start flags DFdepend
    fs <- chkDeps errh flags name
    _ <- dump errh flags t DFdepend dumpnames fs

    -- compile them
    (ok, _, _) <- foldM comp (True, M.empty, M.empty) fs

    when (verb) $
      if ok then
          putStrLnF "All packages are up to date."
      else putStrLnF "All packages compiled (some with errors)."

    return ok

compile_no_deps :: ErrorHandle -> Flags -> String -> IO (Bool)
compile_no_deps errh flags name = do
  (ok, _, _) <- compileFile errh flags M.empty M.empty name
  return ok

-- returns whether the compile errored or not
compileFile :: ErrorHandle -> Flags -> BinMap HeapData -> HashMap -> String ->
               IO (Bool, BinMap HeapData, HashMap)
compileFile errh flags binmap hashmap name_orig = do
    pwd <- getCurrentDirectory
    let name = (createEncodedFullFilePath name_orig pwd)
        name_rel = (getRelativeFilePath name)

    let syntax = (if      hasDotSuf bscSrcSuffix name then CLASSIC
                  else if hasDotSuf bseSrcSuffix name then ESE
                  else BSV)
    setSyntax syntax

    t <- getNow
    let dumpnames = (baseName (dropSuf name), "", "")

    start flags DFcpp
    file <- doCPP errh flags name
    _ <- dumpStr errh flags t DFcpp dumpnames file

    -- ===== the break point between file manipulation and compilation

    -- We don't start and dump this stage because that is handled inside
    -- the "parseSrc" function (since BSV parsing has multiple stages)
    (pkg@(CPackage i _ _ _ _ _), t)
        <- parseSrc (syntax == CLASSIC) errh flags True name file    
    -- [T]: In the case where there is a parsing error, what to do?

    -- when (getIdString i /= baseName (dropSuf name)) $
    --      bsWarning errh
    --          [(noPosition, WFilePackageNameMismatch name_rel (pfpString i))]

    -- dump CSyntax
    when (showCSyntax flags) (putStrLnF (show pkg))
    -- dump stats
    stats flags DFparsed pkg

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
    -- mod is actually never used!
    --putStr (ppReadable mod)
    t <- dump errh flags t DFtypecheck dumpnames mod

    --when (early flags) $ return ()
    let prefix = dirName name ++ "/"

    -- Generate wrapper info for foreign function imports
    -- (this always happens, even when not generating for modules)
    start flags DFgenforeign
    foreign_func_info <- genForeign errh flags prefix mod
    t <- dump errh flags t DFgenforeign dumpnames foreign_func_info

-- TODO HERE Start interesting things
    -- Generate VPI wrappers for foreign function imports
    start flags DFgenVPI
    blurb <- mkGenFileHeader flags
    let ffuncs = map snd foreign_func_info
    vpi_wrappers <- if (backend flags /= Just Verilog)
                    then return []
                    else if (useDPI flags)
                         then genDPIWrappers errh flags prefix blurb ffuncs
                         else genVPIWrappers errh flags prefix blurb ffuncs
    t <- dump errh flags t DFgenVPI dumpnames vpi_wrappers

    -- -- Simplify a little
    start flags DFsimplified
    let mod' = simplify flags mod
    t <- dump errh flags t DFsimplified dumpnames mod'
    stats flags DFsimplified mod'

    -- Read binary interface files
    start flags DFbinary
    let (_, _, impsigs, binmods0, pkgsigs) =
            let findFn i = fromJustOrErr "bsc: binmap" $ M.lookup i binmap
                sorted_ps = [ getIdString i
                               | CImpSign _ _ (CSignature i _ _ _) <- imps ]
            in  unzip5 $ map findFn sorted_ps

    -- injects the "magic" variables genC and genVerilog
    -- should probably be done via primitives
    -- XXX does this interact with signature matching
    -- or will it be caught by flag-matching?
    let adjEnv ::
            [(String, IExpr a)] ->
            (IPackage a) ->
            (IPackage a)
        adjEnv env (IPackage i lps ps ds)
                            | getIdString i == "Prelude" =
                    IPackage i lps ps (map adjDef ds)
            where
                adjDef (IDef i t x p) =
                    case lookup (getIdString (unQualId i)) env of
                        Just e ->  IDef i t e p
                        Nothing -> IDef i t x p
        adjEnv _ p = p

    let
        -- adjust the "raw" packages and then add back their signatures
        -- so they can be put into the current IPackage for linking info
        binmods = zip (map (adjEnv env) binmods0) pkgsigs

    t <- dump errh flags t DFbinary dumpnames binmods
    -- Generate the user-visible type signature
    bi_sig <- genUserSign errh symt mctx


    return ( not tcErrors, binmap, hashmap)


-- Our server should compile subpackets

