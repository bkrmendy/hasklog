{-# LANGUAGE FlexibleContexts #-}

module Hasklog.Compiler (
  compileListing
) where

import Hasklog.Data
import Debug.Trace
import Prelude hiding (Functor)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Writer hiding (Functor)
import Control.Monad.State hiding (Functor)
import Data.Foldable (toList)
import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq, ViewL(..), (><))
import qualified Data.Sequence as Q
import Data.Set (Set)
import qualified Data.Set as S


data SimpleTerm = SVariable Identifier
                | SCompoundTerm Identifier [SimpleTerm]
                deriving (Eq, Ord, Show)

data SimpleClause = SDefiniteClause SimpleTerm [SimpleTerm]
                  | SGoalClause [SimpleTerm]
                  deriving Show

runCompiler :: Writer (Seq WAM) a -> [WAM]
runCompiler = toList . execWriter

variablesOf :: SimpleTerm -> [Identifier]
variablesOf (SVariable v) = [v]
variablesOf (SCompoundTerm _ terms) = concatMap variablesOf terms

goalToDefiniteClause :: [SimpleTerm] -> SimpleClause
goalToDefiniteClause clauseBody = SDefiniteClause clauseHead clauseBody
  where
    variables = concatMap (map SVariable . variablesOf) clauseBody
    clauseHead =  SCompoundTerm "query" variables

simplify :: HornClause -> SimpleClause
simplify (DefiniteClause head body) = SDefiniteClause (simplifyAtoms head) (map simplifyAtoms body)
simplify (GoalClause body)          = SGoalClause (map simplifyAtoms body)

simplifyAtoms :: Term -> SimpleTerm
simplifyAtoms (Atom a)            = SCompoundTerm a []
simplifyAtoms (CompoundTerm f ts) = SCompoundTerm f (map simplifyAtoms ts)
simplifyAtoms (Number a)          = SCompoundTerm (show a) []
simplifyAtoms (Variable v)        = SVariable v

compileListing :: [HornClause] -> Program
compileListing listing = Program compiledPredicates (toFunctor compiledGoal)
  where
    listing' = map simplify listing
    SGoalClause gs = head $ [g | g@(SGoalClause _) <- listing']
    preds = [p | p@(SDefiniteClause _ _) <- listing']
    predicates = foldr (\clause -> M.insertWith (++) (toFunctor clause) [clause]) M.empty (compiledGoal:preds)
    compiledGoal = goalToDefiniteClause gs
    compiledPredicates = map (uncurry compilePredicate) (M.toList predicates)

    toFunctor (SDefiniteClause (SCompoundTerm f ts) _) = Functor f (length ts)
    toFunctor _ = undefined


-- | Compile a predicate into a sequence of WAM instructions.
compilePredicate :: Functor -> [SimpleClause] -> Predicate
compilePredicate f clauses = Predicate f (compileRules clauses)

compileRules :: [SimpleClause] -> [Rule]
compileRules []           = []
compileRules [singleton]  = [compileRule 0 (compileClause singleton)]
compileRules (first:rest) =
    zipWith compileRule [0..] $ compileFirst first : compileRest 1 rest

  where

    compileRest _ []          = undefined -- impossible
    compileRest _ [last]      = [compileLast last]
    compileRest n (next:rest) = compileMiddle n next : compileRest (n + 1) rest

    compileFirst clause =
      do emit (TryMeElse (Label 1))
         compileClause clause
    compileMiddle n clause =
      do emit (RetryMeElse (Label (n + 1)))
         compileClause clause
    compileLast clause =
      do emit TrustMe
         compileClause clause

compileRule :: Int -> Writer (Seq WAM) a -> Rule
compileRule n instructions = Rule (Label n) (runCompiler instructions)

-- | Compile a rule into a sequence of WAM instructions.
compileClause :: SimpleClause -> Writer (Seq WAM) ()
compileClause (SDefiniteClause head body) =
  evalStateT (compileClause' perms thead tbody) emptyRegisterSet
    where
      perms = sharedVars (head:body)
      (thead, tbody) = allocateClause perms head body
      compileClause' perms head body =
        do emit (Allocate (S.size perms))
           compileArgs Get head
           compileCall `mapM_` body
           emit Deallocate
           emit Proceed
-- compileClause (SGoalClause goals) = compileArgs Put (head goals)


emit :: MonadWriter (Seq a) m => a -> m ()
emit inst = tell (Q.singleton inst)


data Mode = Get | Put deriving (Eq, Ord, Show)

type RegisterSet = Set Register

emptyRegisterSet :: RegisterSet
emptyRegisterSet = S.empty

-- | Compile the arguments of a toplevel term in a clause.
compileArgs :: Mode -> TaggedTerm -> StateT RegisterSet (Writer (Seq WAM)) ()
compileArgs mode (TStructure _ _ args) = zipWithM_ (compileArg mode) [1..] args
compileArgs _    _                     = undefined -- TODO: clause heads should always be structures

compileArg :: Mode -> Int -> TaggedTerm -> StateT RegisterSet (Writer (Seq WAM)) ()
compileArg mode n (TVariable reg) =
  let arg = Register n
   in case mode of
        Get -> ifEncountered reg (GetValue reg arg) (GetVariable reg arg)
        Put -> ifEncountered reg (PutValue reg arg) (PutVariable reg arg)
compileArg mode n (TStructure f _ subterms) =
  let -- Replace the register on the argument structure (which is a dummy value in this case) with
      -- the argument register. Then flatten it and compile the resulting expressions.
      flattened = flatten mode (TStructure f (Register n) subterms)
   in compileStruct mode `mapM_` flattened

compileStruct :: Mode -> FlattenedTerm -> StateT RegisterSet (Writer (Seq WAM)) ()
compileStruct mode (FStructure f reg subtermRegs) =
  do modify (S.insert reg)
     let functor = Functor f (length subtermRegs)
     case mode of
       Get -> emit (GetStructure functor reg)
       Put -> emit (PutStructure functor reg)
     compileUnify `mapM_` subtermRegs

compileUnify :: Register -> StateT RegisterSet (Writer (Seq WAM)) ()
compileUnify reg = ifEncountered reg (UnifyValue reg) (UnifyVariable reg)

ifEncountered :: Register -> WAM -> WAM -> StateT RegisterSet (Writer (Seq WAM)) ()
ifEncountered reg t f =
  do encountered <- gets (S.member reg)
     if encountered
       then emit t
       else
         do modify (S.insert reg)
            emit f

compileCall :: TaggedTerm -> StateT RegisterSet (Writer (Seq WAM)) ()
compileCall t@(TStructure f _ args) =
  do compileArgs Put t
     emit (Call (Functor f (length args)))
compileCall _ = undefined


type VariableMap = Map Identifier Int
type VariableSet = Set Identifier

-- NOTE: This skips the optimization that variables that occur only in the head
--   and the _first_ rule can be considered permanent. The special case didn't
--   seem worth the extra complication.
sharedVars :: [SimpleTerm] -> VariableSet
sharedVars terms = mconcat sharedVars'

  where

    sharedVars'  = evalState sharedVars'' M.empty
    sharedVars'' = zipWithM checkShared [1..] terms

    -- The state here maps a variable to the first top-level term in the rule
    -- where it was encountered.
    checkShared :: Int -> SimpleTerm -> State VariableMap VariableSet
    checkShared termNo (SCompoundTerm _ subterms) =
      mconcat <$> mapM (checkShared termNo) subterms
    checkShared termNo (SVariable v) =
      do firstEncountered <- gets (M.findWithDefault termNo v)
         if firstEncountered /= termNo
           then return (S.singleton v)
           else
             do modify (M.insert v termNo)
                return S.empty


data FlattenedTerm = FStructure Identifier Register [Register]

flatten :: Mode -> TaggedTerm -> [FlattenedTerm]
flatten Get = flattenGet
flatten Put = flattenPut

flattenPut :: TaggedTerm -> [FlattenedTerm]
flattenPut = reverse . flattenPut'
  where
    flattenPut' (TVariable _)             = []
    flattenPut' (TStructure f n subterms) =
      FStructure f n (map tag subterms) : concatMap flattenPut' subterms


flattenGet :: TaggedTerm -> [FlattenedTerm]
flattenGet t = flattenGet' (Q.singleton t)
  where
    flattenGet' queue =
      case Q.viewl queue of
        EmptyL        -> []
        first :< rest ->
          case first of
            TVariable _             -> flattenGet' rest
            TStructure f n subterms ->
              FStructure f n (map tag subterms) : flattenGet' (rest >< Q.fromList subterms)


data TaggedTerm = TVariable Register
                | TStructure Identifier Register [TaggedTerm]

tag :: TaggedTerm -> Register
tag (TVariable t)      = t
tag (TStructure _ t _) = t


data Allocation = Allocation {
                    nextFree  :: Int,
                    allocated :: VariableMap
                  }
                deriving (Eq, Ord, Show)

emptyAllocation :: Allocation
emptyAllocation = Allocation 1 M.empty

reserveArgs :: Int -> Allocation
reserveArgs n   = Allocation (n+1) M.empty

allocateClause :: Traversable t => Set Identifier -> SimpleTerm -> t SimpleTerm -> (TaggedTerm, t TaggedTerm)
allocateClause  perms head body = evalState (allocateClause' perms head body) emptyAllocation
  where
    allocateClause' perms head body =
      (,) <$> allocateTop perms head <*> mapM (allocateTop perms) body

allocateTop :: Set Identifier -> SimpleTerm -> StateT Allocation Identity TaggedTerm
allocateTop perms (SCompoundTerm f args) = TStructure f register <$> args'
  where
    -- This register is a dummy value. The toplevel structure does not actually
    -- show up in data movement instructions (it only has to do with the predicate
    -- being called).
    register :: Register
    register = Register (-1)

    args' = evalStateT args'' (reserveArgs (length args))
    args'' = mapM (allocateArg perms) args
allocateTop _ _ = undefined


allocateArg :: VariableSet -> SimpleTerm -> StateT Allocation (State Allocation) TaggedTerm
allocateArg perms (SVariable v)              = TVariable <$> allocateVar perms v
allocateArg perms (SCompoundTerm f subterms) = TStructure f register <$> subterms'
  where
    register :: Register
    register = Register (-1)
    subterms' = mapM (allocate perms) subterms


allocate :: VariableSet -> SimpleTerm -> StateT Allocation (State Allocation) TaggedTerm
allocate perms (SVariable v)              = TVariable <$> allocateVar perms v
allocate perms (SCompoundTerm f subterms) = TStructure f <$> register <*> subterms'
  where
    register = do Allocation next vars <- get
                  put (Allocation (next + 1) vars)
                  return (Register next)
    subterms' = mapM (allocate perms) subterms


-- | Find the storage associated with a variable. Allocate a new location if necessary. The location
--   will be a stack variable if the variable is permanent, or a register if it is temporary.
allocateVar :: VariableSet -> Identifier -> StateT Allocation (State Allocation) Register
allocateVar permanents v =
    -- Determine whether this is a temporary or permanent variable, and construct an appropriate
    -- Register type with the allocated variable.
    if S.member v permanents
      then StackVar <$> lift (allocateVar' v)
      else Register <$> allocateVar' v
  where
    -- Find the register (number) associated with a variable. A new one will be allocated if
    -- necessary.
    allocateVar' v =
      do Allocation next vars <- get
         case M.lookup v vars of
           Just register -> return register
           Nothing ->
             do put (Allocation (next + 1) (M.insert v next vars))
                return next

cFunction :: Functor -> String
cFunction (Functor identifier ar) = identifier ++ "_" ++ show ar ++ "()"

cFile :: Functor -- query
      -> String -- program
      -> String -- source
cFile query program = unlines [
  "#include \"wam.h\""
   , program
   , "void query() {"
   , cFunction query
   , "}"
   , "int main() {"
   , "query();"
   , "report(\"Z\", X(4));"
   , "report(\"W\", X(5));"
   , "return 0;"
   , "}"
   ]

-- WAM Instructions

data Program = Program { _rules :: [Predicate], _goal :: Functor }

data Predicate = Predicate Functor [Rule]

data Rule = Rule Label [WAM]

data WAM = GetStructure Functor  Register
         | GetVariable    Register Register
         | GetValue       Register Register
         | PutStructure   Functor  Register
         | SetVariable    Register
         | SetValue       Register
         | PutVariable    Register Register
         | PutValue       Register Register
         | UnifyVariable  Register
         | UnifyValue     Register
         | Allocate       Int
         | Deallocate
         | Call           Functor
         | Execute        Functor
         | Proceed
         | TryMeElse      Label
         | RetryMeElse    Label
         | TrustMe
         deriving (Eq, Ord, Show)

data Register = Register Int
              | StackVar Int
              deriving (Eq, Ord, Show)

newtype Label = Label Int
              deriving (Eq, Ord, Show)

data Functor = Functor Identifier Int
             deriving (Eq, Ord, Show)

concreteRules :: (s -> String) -> [s] -> String
concreteRules gen = concatMap (\r -> "\n\t" ++ gen r)

instance Syntax Program where

  kind _ = "program"

  describe = kind

  -- TODO: handle goal compilation
  wamAbstractSyntax (Program predicates _) = intercalate "\n\n" (map wamAbstractSyntax predicates)
  cSource (Program predicates goal) = cFile goal (intercalate "\n\n" (map cSource predicates))

instance Syntax Predicate where

  kind _ = "predicate"

  describe (Predicate f _) = "predicate " ++ wamAbstractSyntax f

  wamAbstractSyntax (Predicate f rules) = wamAbstractSyntax f ++ ":" ++ concreteRules wamAbstractSyntax rules
  cSource (Predicate f rules) = "void " ++ cFunction f ++ "{" ++ concreteRules cSource rules ++ "}\n"


instance Syntax Rule where

  kind _ = "rule"

  describe (Rule label _) = "rule " ++ wamAbstractSyntax label

  wamAbstractSyntax (Rule label instructions) = wamAbstractSyntax label ++ ":" ++ concreteInsts
    where
      concreteInsts = concatMap (\i -> "\n\t\t" ++ wamAbstractSyntax i) instructions

  cSource (Rule _ instructions) = concatMap (\i -> "\n\t" ++ cSource i) instructions


instance Syntax WAM where

  kind _ = "WAM instruction"

  wamAbstractSyntax (GetStructure f a)  = delim ["get_structure", wamAbstractSyntax f, wamAbstractSyntax a]
  wamAbstractSyntax (GetVariable  r a)  = delim ["get_variable",  wamAbstractSyntax r, wamAbstractSyntax a]
  wamAbstractSyntax (GetValue     r a)  = delim ["get_value",     wamAbstractSyntax r, wamAbstractSyntax a]
  wamAbstractSyntax (PutStructure f a)  = delim ["put_structure", wamAbstractSyntax f, wamAbstractSyntax a]
  wamAbstractSyntax (PutVariable  r a)  = delim ["put_variable",  wamAbstractSyntax r, wamAbstractSyntax a]
  wamAbstractSyntax (PutValue     r a)  = delim ["put_value",     wamAbstractSyntax r, wamAbstractSyntax a]
  wamAbstractSyntax (UnifyVariable r)   = delim ["unify_variable", wamAbstractSyntax r]
  wamAbstractSyntax (UnifyValue    r)   = delim ["unify_value",    wamAbstractSyntax r]
  wamAbstractSyntax (Allocate n)        = delim ["allocate", show n]
  wamAbstractSyntax Deallocate          = "deallocate"
  wamAbstractSyntax (Call    f)         = delim ["call",    wamAbstractSyntax f]
  wamAbstractSyntax (Execute f)         = delim ["execute", wamAbstractSyntax f]
  wamAbstractSyntax Proceed             = "proceed"
  wamAbstractSyntax (TryMeElse   l)     = delim ["try_me_else",   wamAbstractSyntax l]
  wamAbstractSyntax (RetryMeElse l)     = delim ["retry_me_else", wamAbstractSyntax l]
  wamAbstractSyntax TrustMe             = "trust_me"
  wamAbstractSyntax (SetVariable r)     = delim ["set_variable", wamAbstractSyntax r]
  wamAbstractSyntax (SetValue r)        = delim ["set_value", wamAbstractSyntax r]

  cSource (PutStructure f a)  = unlines ["{", "\t\t" ++ cSource f ++ ";", "\t\tput_structure(f, " ++ cSource a ++ ");", "\t}"]
  cSource (GetStructure f a)  = unlines ["{", "\t\t" ++ cSource f ++ ";", "\t\tget_structure(f, " ++ cSource a ++ ");", "\t}"]
  cSource (SetVariable r)     = "set_variable(" ++ cSource r ++ ");"
  cSource (UnifyVariable r)   = "unify_variable(" ++ cSource r ++ ");"
  cSource (PutVariable r a)   = "put_variable(" ++ cSource r ++ ", " ++ cSource a ++ ");"
  cSource (GetVariable r a)   = "get_variable(" ++ cSource r ++ ", " ++ cSource a ++ ");"
  cSource (SetValue r)        = "set_value(" ++ cSource r ++ ");"
  cSource (UnifyValue r)      = "unify_value(" ++ cSource r ++ ");"
  cSource (PutValue r a)      = "put_value(" ++ cSource r ++ ", " ++ cSource a ++ ");"
  cSource (GetValue r a)      = "get_value(" ++ cSource r ++ ", " ++ cSource a ++ ");"
  cSource (Allocate n)        = "allocate(" ++ show n ++ ");"
  cSource Deallocate          = "deallocate();"
  cSource Proceed             = []
  cSource (Call f)            = cFunction f ++ ";"

  cSource i = "// Not implemented: " ++ show i

  -- set_value
  -- set_variable

delim :: [String] -> String
delim = intercalate "\t"


instance Syntax Label where

  kind _ = "label"

  wamAbstractSyntax (Label l) = show l


instance Syntax Register where

  kind (Register _) = "register"
  kind (StackVar _) = "stack variable"

  wamAbstractSyntax (Register r) = "X" ++ show r
  wamAbstractSyntax (StackVar v) = "Y" ++ show v
 
  cSource (Register r) = "X(" ++ show r ++ ")"
  cSource (StackVar r) = "Y(" ++ show r ++ ")"


instance Syntax Functor where

  kind _ = "functor"

  wamAbstractSyntax (Functor f n) = f ++ "/" ++ show n
  cSource (Functor f n) = "Structure f = { .name = " ++ f ++ ", .arity = " ++ show n ++ " }"
