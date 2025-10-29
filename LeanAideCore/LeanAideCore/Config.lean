import Lean
import Std
import Lean.Util.Trace
open Lean Meta

initialize
  registerTraceClass `Translate.info
  registerTraceClass `Translate.debug
  registerTraceClass `Translate.warning
  registerTraceClass `leanaide.proof.info
  registerTraceClass `leanaide.codegen.info
  registerTraceClass `leanaide.benchmark_embeddings.info

register_option leanaide.logging : Bool :=
  { defValue := false
    descr := "Enable LeanAide logging"
    group := "LeanAide" }

initialize delab_bound : IO.Ref UInt32 ← IO.mkRef 50

class LeanAideBaseDir where
  baseDir : IO System.FilePath

def baseDir [inst: LeanAideBaseDir] : IO System.FilePath := do
  inst.baseDir

variable [LeanAideBaseDir]

def leanAideLogging? : CoreM (Option String) := do
  let loggingEnabled : Bool := leanaide.logging.get (← getOptions)
  if loggingEnabled then return some "1"
  else IO.getEnv "LEANAIDE_LOGGING"

def leanAideLoggingIO? : IO (Option String) := do
  IO.getEnv "LEANAIDE_LOGGING"

def logHandle : IO IO.FS.Handle := do
  let logPath : System.FilePath :=
    (← baseDir) / ".lake/build/lib/leanaide.log"
  IO.FS.Handle.mk logPath IO.FS.Mode.append

def logTimed (message: String) : IO Unit := do
  match (← leanAideLoggingIO?) with
  | some "0" =>
    return ()
  | some _   => let handle ← logHandle
                let time ← IO.monoMsNow
                let message := s!"[{time}]  {message}"
                IO.FS.Handle.putStrLn handle message
                IO.FS.Handle.flush handle
  | _ =>
    return ()


def resourcesDir : IO System.FilePath := do
  let base ← baseDir
  return base / "resources"

-- #eval resourcesDir

-- def logToStderr (cls : Name) (msg : String) : IO Unit :=
--   IO.eprintln s!"[{cls}] {msg}"

-- def logToFile (cls : Name) (msg : String) : IO Unit :=
--   let fname := ["D:", "fun-dev", "LeanAide", "test.log"]
--   IO.FS.writeFile (System.mkFilePath fname) (cls.toString ++ ".log" ++ msg ++ "\n")

set_option diagnostics true

-- Step 1: Define the log state to track enabled classes
structure LogConfig where
  stderr : Bool := false
  file : Bool := false
deriving Inhabited, BEq

-- Step 2 : Create environment extension to store per-class config
initialize logConfigExt : SimpleScopedEnvExtension (Name × LogConfig) (Std.HashMap Name LogConfig) ←
  registerSimpleScopedEnvExtension {
    addEntry := fun map (cls, config) => map.insert cls config
    initial := {}
  }

-- Step 3 : Register Trace classes
initialize registerTraceClass `benchmark_embeddings.info

-- Step 4 : Auto-configure logging based on trace options
def getLogConfig (cls : Name) : CoreM LogConfig := do
  let opts ← getOptions
  let traceEnabled := opts.getBool (Name.mkStr1 "trace" ++ cls) false

  if traceEnabled then
    -- Auto-enable both if trace is on
    return { stderr := true, file := true }
  else
    -- Check environment extension for manual config
    let env ← getEnv
    let configs := logConfigExt.getState env
    return configs.getD cls { stderr := false, file := false }

-- Store config in environment extension
def setLogConfig (cls : Name) (stderr file : Bool) : CoreM Unit := do
  modifyEnv
    <| fun env =>
        logConfigExt.addEntry env ⟨cls, { stderr:=stderr, file:=file }⟩

-- CoreM runner with complete context and state
def runCoreM {α : Type} [MonadEnv IO] (x : CoreM α) : IO α := do
  let env ← getEnv
  let ctx : Core.Context := {
    fileName := "<logging>"
    fileMap := FileMap.ofString ""
    options := {}
    currNamespace := Name.anonymous
    openDecls := []
    initHeartbeats := 0
  }

  let state : Core.State := {
    env := env
    nextMacroScope := firstFrontendMacroScope + 1
    ngen := { namePrefix := `_uniq }
    traceState := {}
    cache := {}
    messages := {}
  }

  let (result, _) ← x.toIO ctx state
  return result

-- Step 5 : Simple logging function
def logTrace [MonadEnv IO] (cls : Name) (msg : String) : IO Unit := do
  -- Create a minimal CoreM context

  let localConfig ← runCoreM <| getLogConfig cls
  let stderrEnabled :=
    if localConfig.stderr then s!"[{cls}] {msg}"
    else ""
  let fileEnabled ←
    if localConfig.file then
      let fname := s!"{cls.toString.replace "." "_"}.log"
      IO.FS.writeFile fname s!"{msg}\n"
    else
      pure ()

def enableLogging [MonadEnv IO] (cls : Name) (stderr file : Bool) : IO Unit := do
  runCoreM <| setLogConfig cls stderr file

  -- let ctx : Core.Context := {
  --   fileName := "<logging>"
  --   fileMap := default
  --   options := {}
  --   currNamespace := Name.anonymous
  --   openDecls := []
  --   initHeartbeats := 0
  -- }

  -- let (config, _) ← getLogConfig cls |>.toIO ctx {}

  -- if config.stderrEnabled then
  --   IO.eprintln s!"[{cls}] {msg}"

-- -- Step 2: Create global state for logging control
-- initialize logState : IO.Ref LogState ← IO.mkRef ⟨[], []⟩

-- -- Step 3: Helper functions to enable/disable logging per class
-- def enableStderrLogging (cls : Name) : IO Unit := do
--   logState.modify fun s => { s with stderrClasses := cls :: s.stderrClasses }

-- def enableFileLogging (cls : Name) : IO Unit := do
--   logState.modify fun s => { s with fileClasses := cls :: s.fileClasses }

-- def disableStderrLogging (cls : Name) : IO Unit := do
--   logState.modify fun s => { s with stderrClasses := s.stderrClasses.erase cls }

-- def disableFileLogging (cls : Name) : IO Unit := do
--   logState.modify fun s => { s with fileClasses := s.fileClasses.erase cls }

-- -- Step 4: Check functions
-- def shouldLogToStderr (cls : Name) : IO Bool := do
--   let state ← logState.get
--   return state.stderrClasses.contains cls

-- def shouldLogToFile (cls : Name) : IO Bool := do
--   let state ← logState.get
--   return state.fileClasses.contains cls
-- abbrev LoggingIO := StateT LogState IO

-- def logTrace (cls : Name) (msg : String) : IO Unit := do
--   if ← shouldLogToStderr cls then
--     IO.eprintln s!"[{cls}] {msg}"
--   if ← shouldLogToFile cls then
--     IO.FS.writeFile s!"{cls}.log" s!"{msg}\n"

-- def setupLogging : IO Unit := do
--   enableStderrLogging `leanaide.benchmark_embeddings.info
--   enableFileLogging `leanaide.benchmark_embeddings.info
