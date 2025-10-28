import Lean
import Std
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

-- Step 1: Define the log state to track enabled classes
structure LogState where
  stderrClasses : List Name := []
  fileClasses : List Name := []
  deriving Inhabited

-- Step 2: Create global state for logging control
initialize logState : IO.Ref LogState ← IO.mkRef ⟨[], []⟩

-- Step 3: Helper functions to enable/disable logging per class
def enableStderrLogging (cls : Name) : IO Unit := do
  logState.modify fun s => { s with stderrClasses := cls :: s.stderrClasses }

def enableFileLogging (cls : Name) : IO Unit := do
  logState.modify fun s => { s with fileClasses := cls :: s.fileClasses }

def disableStderrLogging (cls : Name) : IO Unit := do
  logState.modify fun s => { s with stderrClasses := s.stderrClasses.erase cls }

def disableFileLogging (cls : Name) : IO Unit := do
  logState.modify fun s => { s with fileClasses := s.fileClasses.erase cls }

-- Step 4: Check functions
def shouldLogToStderr (cls : Name) : IO Bool := do
  let state ← logState.get
  return state.stderrClasses.contains cls

def shouldLogToFile (cls : Name) : IO Bool := do
  let state ← logState.get
  return state.fileClasses.contains cls
abbrev LoggingIO := StateT LogState IO

def logTrace (cls : Name) (msg : String) : IO Unit := do
  if ← shouldLogToStderr cls then
    IO.eprintln s!"[{cls}] {msg}"
  if ← shouldLogToFile cls then
    IO.FS.writeFile s!"{cls}.log" s!"{msg}\n"
