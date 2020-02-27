{-# LANGUAGE LambdaCase #-}
module Eval where

import Data.List (foldl', null, isSuffixOf, intercalate)
import Data.List.Split (splitOn, splitWhen)
import Control.Monad.State
import Control.Monad.State.Lazy (StateT(..), runStateT, liftIO, modify, get, put)
import System.Exit (exitSuccess, exitFailure, exitWith, ExitCode(..))
import System.Process (readProcessWithExitCode)
import Control.Concurrent (forkIO)
import qualified Data.Map as Map
import Data.Maybe (fromJust, mapMaybe, isJust, Maybe(..))
import Control.Exception
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as ParsecError

import Parsing
import Emit
import Obj
import Types
import Infer
import Deftype
import Sumtypes
import ColorText
import Template
import Util
import Commands
import Expand
import Lookup
import Qualify
import TypeError
import Concretize
import Path
import Primitives

-- | Dynamic (REPL) evaluation of XObj:s (s-expressions)
eval :: Env -> XObj -> StateT Context IO (Either EvalError XObj)
eval env xobj@(XObj o i t) =
  case o of
    Lst body   -> eval' body
    Sym path@(SymPath p n) _ -> do
      ctx <- get
      let fppl = projectFilePathPrintLength (contextProj ctx)
      case lookupInEnv (SymPath ("Dynamic" : p) n) env of
        Just (_, Binder _ found) -> return (Right (resolveDef found))
        Nothing ->
          case lookupInEnv path env of
            Just (_, Binder _ found) -> return (Right (resolveDef found))
            Nothing ->
              return (makeEvalError ctx Nothing
                                    ("Can't find symbol '" ++ show path ++ "'")
                                    (info xobj))
    Arr objs  -> do
      evaled <- fmap sequence (mapM (eval env) objs)
      return $ do ok <- evaled
                  Right (XObj (Arr ok) i t)
    _        -> return (Right xobj)
  where
    resolveDef (XObj (Lst [XObj DefDynamic _ _, _, value]) _ _) = value
    resolveDef x = x
    eval' = \case
      [XObj (Sym (SymPath ["Dynamic"] "and") _) _ _, a, b] ->
        do ctx <- get
           evaledA <- eval env a
           evaledB <- eval env b
           return $ do okA <- evaledA
                       case okA of
                         XObj (Bol ab) _ _ ->
                           if ab
                             then do okB <- evaledB
                                     case okB of
                                       XObj (Bol bb) _ _ ->
                                         if bb then Right trueXObj else Right falseXObj
                                       _ ->
                                         makeEvalError ctx Nothing ("Can’t perform call `and` on " ++ pretty okB) (info okB)
                             else Right falseXObj
                         _ ->
                           makeEvalError ctx Nothing ("Can’t call `and` on " ++ pretty okA) (info okA)

      [XObj (Sym (SymPath ["Dynamic"] "or") _) _ _, a, b] ->
        do ctx <- get
           evaledA <- eval env a
           evaledB <- eval env b
           return $ do okA <- evaledA
                       case okA of
                         XObj (Bol ab) _ _ ->
                           if ab
                             then Right trueXObj
                             else do okB <- evaledB
                                     case okB of
                                       XObj (Bol bb) _ _ ->
                                         if bb then Right trueXObj else Right falseXObj
                                       _ ->
                                         makeEvalError ctx Nothing ("Can’t call `or` on " ++ pretty okB) (info okB)
                         _ ->
                           makeEvalError ctx Nothing ("Can’t call `or` on " ++ pretty okA) (info okA)
      [XObj If _ _, mcond, mtrue, mfalse] -> do
        evd <- eval env mcond
        case evd of
          Right cond ->
            case obj cond of
              Bol b -> eval env (if b then mtrue else mfalse)
              _     -> do
                ctx <- get
                return (makeEvalError ctx Nothing
                         ("This `if` condition contains non-boolean value: " ++
                          pretty cond) (info cond))
          Left e -> return (Left e)
      x@(XObj If _ _:_) -> do
        ctx <- get
        return (makeEvalError ctx Nothing
                 ("I didn’t understand this `if`.\n\n Got:\n```\n" ++ pretty xobj ++
                  "\n```\n\nExpected the form:\n```\n(if cond then else)\n```\n")
                 (info xobj))
      l@[XObj Fn{} _ _, args@(XObj (Arr a) _ _), f] -> do
        ctx <- get
        if all isUnqualifiedSym a
        then return (Right (XObj (Closure (XObj (Lst l) i t) (CEnv env)) i t))
        else return (makeEvalError ctx Nothing ("`fn` requires all arguments to be unqualified symbols, but it got `" ++ pretty args ++ "`") (info xobj))
      x@(XObj sym@(Sym s _) _ _):args ->
        case Map.lookup s primitives of
          Just prim -> prim x env args
          Nothing -> do
            ctx <- get
            f <- eval env x
            case f of
               Right (XObj (Closure (XObj (Lst [XObj (Fn _ _) _ _, XObj (Arr params) _ _, body]) _ _) (CEnv e)) _ _) ->
                 case checkArity sym params args of
                   Left err ->
                     return (makeEvalError ctx Nothing err (info x))
                   Right () ->
                     do evaledArgs <- fmap sequence (mapM (eval env) args)
                        case evaledArgs of
                          Right okArgs -> apply e body params okArgs
                          Left err -> return (Left err)

               Right (XObj (Lst [XObj Dynamic _ _, _, XObj (Arr params) _ _, body]) _ _) ->
                 case checkArity sym params args of
                   Left err ->
                     return (makeEvalError ctx Nothing err (info x))
                   Right () ->
                     do evaledArgs <- fmap sequence (mapM (eval env) args)
                        case evaledArgs of
                          Right okArgs -> apply env body params okArgs
                          Left err -> return (Left err)

               Right (XObj (Lst [XObj Macro _ _, _, XObj (Arr params) _ _, body]) _ _) ->
                 case checkArity sym params args of
                   Left err ->
                     return (makeEvalError ctx Nothing err (info x))
                   Right () ->
                     -- Replace info so that the macro which is called gets the source location info of the expansion site.
                     --let replacedBody = replaceSourceInfoOnXObj (info xobj) body
                     -- TODO: fix expansion here
                     apply env body params args

               Right (XObj (Lst [XObj (Command callback) _ _, _]) _ _) ->
                 do evaledArgs <- fmap sequence (mapM (eval env) args)
                    case evaledArgs of
                      Right okArgs -> getCommand callback okArgs
                      Left err -> return (Left err)
               Right val ->
                 return (makeEvalError ctx Nothing
                          ("You are trying to call a non-callable `" ++ show s ++ "`.")
                          (info x))
               Left err -> return (Left err)

      l@(XObj (Lst [XObj (Fn _ _) _ _, XObj (Arr params) _ _, body]) _ _):args -> do
        ctx <- get
        f <- eval env l
        case f of
           Right (XObj (Closure (XObj (Lst [XObj (Fn _ _) _ _, XObj (Arr params) _ _, body]) _ _) (CEnv e)) _ _) ->
             case checkArity (pretty l) params args of
               Left err ->
                 return (makeEvalError ctx Nothing err (info l))
               Right () ->
                 do evaledArgs <- fmap sequence (mapM (eval env) args)
                    case evaledArgs of
                      Right okArgs -> apply e body params okArgs
                      Left err -> return (Left err)
           x -> return x
      x -> do
        ctx <- get
        return (makeEvalError ctx Nothing
                 ("I did not understand the form `" ++ show x ++ "`.")
                 (info xobj))
    checkArity s params args =
      let la = length args
          lp = length params
      in if lp == la
         then Right ()
         else if la < lp
              then Left ("`" ++ show s ++ "` expects " ++ show lp ++
                         " but received only " ++ show la ++
                         ".\n\nYou’ll have to provide " ++
                         intercalate ", " (map pretty (drop la params)) ++
                         " as well.")
              else Left ("`" ++ show s ++ "` expects " ++ show lp ++
                         " but received " ++ show la ++ ".\n\nThe arguments " ++
                         intercalate ", " (map pretty (drop lp args)) ++
                         " are not needed.")

apply :: Env -> XObj -> [XObj] -> [XObj] -> StateT Context IO (Either EvalError XObj)
apply env body params args =
  let allParams = map getName params
  in case splitWhen ((==) ":rest") allParams of
       [a, b] -> callWith a b
       [a] -> callWith a []
       _ -> do
        ctx <- get
        return (makeEvalError ctx Nothing
                 ("I didn’t understand this macro’s argument split, got `" ++
                  joinWith "," allParams ++
                  "`, but expected exactly one `:rest` separator.")
                 (info (head params)))
  where callWith proper rest =
          let n = length proper
              insideEnv = Env Map.empty (Just env) Nothing [] InternalEnv 0
              insideEnv' = foldl' (\e (p, x) -> extendEnv e p x) insideEnv
                                  (zip proper (take n args))
              insideEnv'' = if null rest
                             then insideEnv'
                             else extendEnv insideEnv'
                                   (head rest)
                                   (XObj (Lst (drop n args)) Nothing Nothing)
          in eval insideEnv'' body

-- LEGACY STUFF

-- | Print a found binder.
found binder =
  liftIO $ do putStrLnWithColor White (show binder)
              return dynamicNil

-- | A command at the REPL
-- | TODO: Is it possible to remove the error cases?
data ReplCommand = ReplParseError String XObj
                 | ReplEval XObj
                 | ListOfCallbacks [CommandCallback]

-- | Parses a string and then converts the resulting forms to commands, which are evaluated in order.
executeString :: Bool -> Context -> String -> String -> IO Context
executeString doCatch ctx input fileName = if doCatch then catch exec (catcher ctx) else exec
  where exec = case parse input fileName of
                 Left parseError ->
                   let sourcePos = Parsec.errorPos parseError
                       parseErrorXObj = XObj (Lst []) (Just dummyInfo { infoFile = fileName
                                                                      , infoLine = Parsec.sourceLine sourcePos
                                                                      , infoColumn = Parsec.sourceColumn sourcePos
                                                                      }) Nothing
                   in  executeCommand ctx (ReplParseError (replaceChars (Map.fromList [('\n', " ")]) (show parseError)) parseErrorXObj)
                 Right xobjs -> foldM folder ctx xobjs

-- | Used by functions that has a series of forms to evaluate and need to fold over them (producing a new Context in the end)
folder :: Context -> XObj -> IO Context
folder context xobj =
  do cmd <- objToCommand context xobj
     executeCommand context cmd

-- | Take a ReplCommand and execute it.
executeCommand :: Context -> ReplCommand -> IO Context
executeCommand ctx@(Context env typeEnv pathStrings proj lastInput execMode) cmd =
  do when (isJust (envModuleName env)) $
       error ("Global env module name is " ++ fromJust (envModuleName env) ++ " (should be Nothing).")
     case cmd of
       ReplEval xobj ->
         do (result, newCtx) <- runStateT (eval env xobj) ctx
            case result of
              Left e ->
                reportExecutionError newCtx (show e)
              Right (XObj (Lst []) _ _) ->
                -- Nil result won't print
                return newCtx
              Right result ->
                do putStrLnWithColor Yellow ("=> " ++ pretty result)
                   return newCtx
       -- TODO: This is a weird case:
       ReplParseError e xobj ->
         do let msg =  "[PARSE ERROR] " ++ e
                fppl = projectFilePathPrintLength (contextProj ctx)
            case contextExecMode ctx of
              Check -> putStrLn (machineReadableInfoFromXObj fppl xobj ++ " " ++ msg)
              _ -> putStrLnWithColor Red msg
            throw CancelEvaluationException
       ListOfCallbacks callbacks -> foldM (\ctx' cb -> callCallbackWithArgs ctx' cb []) ctx callbacks

reportExecutionError :: Context -> String -> IO Context
reportExecutionError ctx errorMessage =
  case contextExecMode ctx of
    Check ->
      do putStrLn errorMessage
         return ctx
    _ ->
      do putStrLnWithColor Red errorMessage
         throw CancelEvaluationException

-- | Call a CommandCallback.
callCallbackWithArgs :: Context -> CommandCallback -> [XObj] -> IO Context
callCallbackWithArgs ctx callback args =
  do (ret, newCtx) <- runStateT (callback args) ctx
     let fppl = projectFilePathPrintLength (contextProj newCtx)
     case ret of
       Left err -> throw (EvalException (err fppl))
       Right _ -> return newCtx

-- | Convert an XObj to a ReplCommand so that it can be executed dynamically.
-- | TODO: Does this function need the Context?
objToCommand :: Context -> XObj -> IO ReplCommand
objToCommand ctx (XObj (Sym (SymPath [] (':' : text)) _) _ _) =
  return (ListOfCallbacks (mapMaybe charToCommand text))
objToCommand ctx xobj =
  return (ReplEval xobj)

-- | Generate commands from shortcut characters (i.e. 'b' = build)
charToCommand :: Char -> Maybe CommandCallback
charToCommand 'x' = Just commandRunExe
charToCommand 'r' = Just commandReload
charToCommand 'b' = Just (commandBuild False)
charToCommand 'c' = Just commandCat
charToCommand 'e' = Just commandListBindings
charToCommand 'h' = Just commandHelp
charToCommand 'p' = Just commandProject
charToCommand 'q' = Just commandQuit
charToCommand _   = Just (\_ -> return dynamicNil)

-- | Decides what to do when the evaluation fails for some reason.
catcher :: Context -> CarpException -> IO Context
catcher ctx exception =
  case exception of
    (ShellOutException message returnCode) ->
      do putStrLnWithColor Red ("[RUNTIME ERROR] " ++ message)
         stop returnCode
    CancelEvaluationException ->
      stop 1
    EvalException evalError ->
      do putStrLnWithColor Red (show evalError)
         stop 1
  where stop returnCode =
          case contextExecMode ctx of
            Repl -> return ctx
            Build -> exitWith (ExitFailure returnCode)
            Install _ -> exitWith (ExitFailure returnCode)
            BuildAndRun -> exitWith (ExitFailure returnCode)
            Check -> exitSuccess

specialCommandDefine :: XObj -> StateT Context IO (Either EvalError XObj)
specialCommandDefine xobj =
  do result <- annotateWithinContext True xobj
     case result of
       Right (annXObj, annDeps) ->
         do ctxAfterExpansion <- get
            ctxWithDeps <- liftIO $ foldM (define True) ctxAfterExpansion annDeps
            ctxWithDef <- liftIO $ define False ctxWithDeps annXObj
            put ctxWithDef
            return dynamicNil
       Left err ->
         return (Left err)

annotateWithinContext :: Bool -> XObj -> StateT Context IO (Either EvalError (XObj, [XObj]))
annotateWithinContext qualifyDefn xobj =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         globalEnv = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         innerEnv = getEnv globalEnv pathStrings
     expansionResult <- expandAll eval globalEnv xobj
     ctxAfterExpansion <- get
     case expansionResult of
       Left err -> return (makeEvalError ctx Nothing (show err) Nothing)
       Right expanded ->
         let xobjFullPath = if qualifyDefn then setFullyQualifiedDefn expanded (SymPath pathStrings (getName xobj)) else expanded
             xobjFullSymbols = setFullyQualifiedSymbols typeEnv globalEnv innerEnv xobjFullPath
         in case annotate typeEnv globalEnv xobjFullSymbols of
              Left err ->
                case contextExecMode ctx of
                  Check ->
                    let fppl = projectFilePathPrintLength (contextProj ctx)
                    in  return (Left (EvalError (joinWith "\n" (machineReadableErrorStrings fppl err)) Nothing fppl))
                  _ ->
                    return (Left (EvalError (show err) Nothing fppl))
              Right ok ->
                return (Right ok)

dynamicOrMacroWith :: (SymPath -> [XObj]) -> Ty -> String -> XObj -> StateT Context IO (Either EvalError XObj)
dynamicOrMacroWith producer ty name body =
  do ctx <- get
     let pathStrings = contextPath ctx
         globalEnv = contextGlobalEnv ctx
         path = SymPath pathStrings name
         elem = XObj (Lst (producer path)) (info body) (Just ty)
         meta = existingMeta globalEnv elem
     put (ctx { contextGlobalEnv = envInsertAt globalEnv path (Binder meta elem) })
     return dynamicNil

dynamicOrMacro :: Obj -> Ty -> String -> XObj -> XObj -> StateT Context IO (Either EvalError XObj)
dynamicOrMacro pat ty name params body =
  dynamicOrMacroWith (\path -> [XObj pat Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing, params, body]) ty name body

specialCommandDefndynamic :: String -> XObj -> XObj -> StateT Context IO (Either EvalError XObj)
specialCommandDefndynamic = dynamicOrMacro Dynamic DynamicTy

specialCommandDefdynamic :: String -> XObj -> StateT Context IO (Either EvalError XObj)
specialCommandDefdynamic name body =
  do env <- gets contextGlobalEnv
     result <- eval env body
     case result of
       Left err -> return (Left err)
       Right evaledBody ->
         dynamicOrMacroWith (\path -> [XObj DefDynamic Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing, evaledBody]) DynamicTy name body

specialCommandDefmacro :: String -> XObj -> XObj -> StateT Context IO (Either EvalError XObj)
specialCommandDefmacro = dynamicOrMacro Macro MacroTy

specialCommandDefmodule :: XObj -> String -> [XObj] -> StateT Context IO (Either EvalError XObj)
specialCommandDefmodule xobj moduleName innerExpressions =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         env = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         lastInput = contextLastInput ctx
         execMode = contextExecMode ctx
         proj = contextProj ctx

         defineIt :: MetaData -> StateT Context IO (Either EvalError XObj)
         defineIt meta = do let parentEnv = getEnv env pathStrings
                                innerEnv = Env (Map.fromList []) (Just parentEnv) (Just moduleName) [] ExternalEnv 0
                                newModule = XObj (Mod innerEnv) (info xobj) (Just ModuleTy)
                                globalEnvWithModuleAdded = envInsertAt env (SymPath pathStrings moduleName) (Binder meta newModule)
                                ctx' = Context globalEnvWithModuleAdded typeEnv (pathStrings ++ [moduleName]) proj lastInput execMode
                            ctxAfterModuleDef <- liftIO $ foldM folder ctx' innerExpressions
                            put (popModulePath ctxAfterModuleDef)
                            return dynamicNil

     result <- case lookupInEnv (SymPath pathStrings moduleName) env of
                 Just (_, Binder _ (XObj (Mod _) _ _)) ->
                   do let ctx' = Context env typeEnv (pathStrings ++ [moduleName]) proj lastInput execMode -- use { = } syntax instead
                      ctxAfterModuleAdditions <- liftIO $ foldM folder ctx' innerExpressions
                      put (popModulePath ctxAfterModuleAdditions)
                      return dynamicNil -- TODO: propagate errors...
                 Just (_, Binder existingMeta (XObj (Lst [XObj DocStub _ _, _]) _ _)) ->
                   defineIt existingMeta
                 Just (_, Binder _ x) ->
                   return (makeEvalError ctx Nothing ("Can't redefine '" ++ moduleName ++ "' as module") (info xobj))
                 Nothing ->
                   defineIt emptyMeta

     case result of
       Left err -> return (Left err)
       Right _ -> return dynamicNil

specialCommandInfo :: XObj -> StateT Context IO (Either EvalError XObj)
specialCommandInfo target@(XObj (Sym path@(SymPath _ name) _) _ _) =
  do ctx <- get
     let env = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         proj = contextProj ctx
         execMode = contextExecMode ctx
         printer allowLookupInALL binderPair itIsAnErrorNotToFindIt =
           case binderPair of
             Just (_, binder@(Binder metaData x@(XObj _ (Just i) _))) ->
               do putStrLn (show binder ++ "\nDefined at " ++ prettyInfo i)
                  case Map.lookup "doc" (getMeta metaData) of
                    Just (XObj (Str val) _ _) -> putStrLn ("Documentation: " ++ val)
                    Nothing -> return ()
                  when (projectPrintTypedAST proj) $ putStrLnWithColor Yellow (prettyTyped x)
             Just (_, binder@(Binder metaData x)) ->
               do print binder
                  case Map.lookup "doc" (getMeta metaData) of
                    Just (XObj (Str val) _ _) -> putStrLn ("Documentation: " ++ val)
                    Nothing -> return ()
                  when (projectPrintTypedAST proj) $ putStrLnWithColor Yellow (prettyTyped x)
             Nothing ->
               when allowLookupInALL
               (case multiLookupALL name env of
                 [] ->
                   when itIsAnErrorNotToFindIt $
                     putStrLn $
                       case execMode of
                         Check -> let fppl = projectFilePathPrintLength (contextProj ctx)
                                  in  machineReadableInfoFromXObj fppl target ++ (" Can't find '" ++ show path ++ "'")
                         _ -> strWithColor Red ("Can't find '" ++ show path ++ "'")
                 binders ->
                   mapM_ (\(env, binder@(Binder _ (XObj _ i _))) ->
                            case i of
                              Just i' -> putStrLnWithColor White (show binder ++ " Defined at " ++ prettyInfo i')
                              Nothing -> putStrLnWithColor White (show binder))
                         binders)
     case path of
       SymPath [] _ ->
         -- First look in the type env, then in the global env:
         do case lookupInEnv path (getTypeEnv typeEnv) of
              Nothing -> liftIO (printer True (lookupInEnv path env) True)
              found -> do liftIO (printer True found True) -- this will print the interface itself
                          liftIO (printer True (lookupInEnv path env) False) -- this will print the locations of the implementers of the interface
            return dynamicNil
       qualifiedPath ->
         case lookupInEnv path env of
           Nothing -> notFound target path
           found -> do liftIO (printer False found True)
                       return dynamicNil

specialCommandType :: XObj -> StateT Context IO (Either EvalError XObj)
specialCommandType target =
  do ctx <- get
     let env = contextGlobalEnv ctx
     case target of
           XObj (Sym path@(SymPath [] name) _) _ _ ->
             case lookupInEnv path env of
               Just (_, binder) ->
                 found binder
               Nothing ->
                 case multiLookupALL name env of
                   [] ->
                     notFound target path
                   binders ->
                     liftIO $ do mapM_ (\(env, binder) -> putStrLnWithColor White (show binder)) binders
                                 return dynamicNil
           XObj (Sym qualifiedPath _) _ _ ->
             case lookupInEnv qualifiedPath env of
               Just (_, binder) ->
                 found binder
               Nothing ->
                 notFound target qualifiedPath
           _ ->
             liftIO $ do putStrLnWithColor Red ("Can't get the type of non-symbol: " ++ pretty target)
                         return dynamicNil

specialCommandMembers :: XObj -> Env -> StateT Context IO (Either EvalError XObj)
specialCommandMembers target env =
  do ctx <- get
     let typeEnv = contextTypeEnv ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
     case bottomedTarget target of
           XObj (Sym path@(SymPath _ name) _) _ _ ->
              case lookupInEnv path (getTypeEnv typeEnv) of
                Just (_, Binder _ (XObj (Lst [
                  XObj (Deftype structTy) Nothing Nothing,
                  XObj (Sym (SymPath pathStrings typeName) Symbol) Nothing Nothing,
                  XObj (Arr members) _ _]) _ _))
                  ->
                    return (Right (XObj (Arr (map (\(a, b) -> XObj (Lst [a, b]) Nothing Nothing) (pairwise members))) Nothing Nothing))
                Just (_, Binder _ (XObj (Lst (
                  XObj (DefSumtype structTy) Nothing Nothing :
                  XObj (Sym (SymPath pathStrings typeName) Symbol) Nothing Nothing :
                  sumtypeCases)) _ _))
                  ->
                    return (Right (XObj (Arr (concatMap getMembersFromCase sumtypeCases)) Nothing Nothing))
                  where getMembersFromCase :: XObj -> [XObj]
                        getMembersFromCase (XObj (Lst members) _ _) =
                          map (\(a, b) -> XObj (Lst [a, b]) Nothing Nothing) (pairwise members)
                        getMembersFromCase x@(XObj (Sym sym _) _ _) =
                          [XObj (Lst [x, XObj (Arr []) Nothing Nothing]) Nothing Nothing]
                        getMembersFromCase (XObj x _ _) =
                          error ("Can't handle case " ++ show x)
                _ ->
                  return (makeEvalError ctx Nothing ("Can't find a struct type named '" ++ name ++ "' in type environment") (info target))
           _ -> return (makeEvalError ctx Nothing ("Can't get the members of non-symbol: " ++ pretty target) (info target))
  where bottomedTarget target =
          case target of
            XObj (Sym targetPath _) _ _ ->
              case lookupInEnv targetPath env of
                -- this is a trick: every type generates a module in the env;
                -- we’re special-casing here because we need the parent of the
                -- module
                Just (_, Binder _ (XObj (Mod _) _ _)) -> target
                -- if we’re recursing into a non-sym, we’ll stop one level down
                Just (_, Binder _ x) -> bottomedTarget x
                _ -> target
            _ -> target

specialCommandUse :: XObj -> SymPath -> StateT Context IO (Either EvalError XObj)
specialCommandUse xobj path =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         env = contextGlobalEnv ctx
         e = getEnv env pathStrings
         useThese = envUseModules e
         e' = if path `elem` useThese then e else e { envUseModules = path : useThese }
         innerEnv = getEnv env pathStrings -- Duplication of e?
     case lookupInEnv path innerEnv of
       Just (_, Binder _ _) ->
         do put $ ctx { contextGlobalEnv = envReplaceEnvAt env pathStrings e' }
            return dynamicNil
       Nothing ->
         return (makeEvalError ctx Nothing ("Can't find a module named '" ++ show path ++ "'") (info xobj))

specialCommandWith :: XObj -> SymPath -> [XObj] -> StateT Context IO (Either EvalError XObj)
specialCommandWith xobj path forms =
  do ctx <- get
     let pathStrings = contextPath ctx
         env = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
         useThese = envUseModules env
         env' = if path `elem` useThese then env else env { envUseModules = path : useThese }
         ctx' = ctx { contextGlobalEnv = env' }
     ctxAfter <- liftIO $ foldM folder ctx' forms
     let envAfter = contextGlobalEnv ctxAfter
         ctxAfter' = ctx { contextGlobalEnv = envAfter { envUseModules = useThese } } -- This will undo ALL use:s made inside the 'with'.
     put ctxAfter'
     return dynamicNil

-- | Set meta data for a Binder
specialCommandMetaSet :: SymPath -> String -> XObj -> StateT Context IO (Either EvalError XObj)
specialCommandMetaSet path key value =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         globalEnv = contextGlobalEnv ctx
     case lookupInEnv (consPath pathStrings path) globalEnv of
       Just (_, binder@(Binder _ xobj)) ->
         -- | Set meta on existing binder
         setMetaOn ctx binder
       Nothing ->
         case path of
           -- | If the path is unqualified, create a binder and set the meta on that one. This enables docstrings before function exists.
           (SymPath [] name) ->
             setMetaOn ctx (Binder emptyMeta (XObj (Lst [XObj DocStub Nothing Nothing,
                                                         XObj (Sym (SymPath pathStrings name) Symbol) Nothing Nothing])
                                              (Just dummyInfo)
                                              (Just (VarTy "a"))))
           (SymPath _ _) ->
             return (makeEvalError ctx Nothing ("Special command 'meta-set!' failed, can't find '" ++ show path ++ "'") (info value))
       where
         setMetaOn :: Context -> Binder -> StateT Context IO (Either EvalError XObj)
         setMetaOn ctx binder@(Binder metaData xobj) =
           do let globalEnv = contextGlobalEnv ctx
                  newMetaData = MetaData (Map.insert key value (getMeta metaData))
                  xobjPath = getPath xobj
                  newBinder = binder { binderMeta = newMetaData }
                  newEnv = envInsertAt globalEnv xobjPath newBinder
              put (ctx { contextGlobalEnv = newEnv })
              return dynamicNil

-- | Get meta data for a Binder
specialCommandMetaGet :: SymPath -> String -> StateT Context IO (Either EvalError XObj)
specialCommandMetaGet path key =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
         globalEnv = contextGlobalEnv ctx
     case lookupInEnv (consPath pathStrings path) globalEnv of
       Just (_, Binder metaData _) ->
           case Map.lookup key (getMeta metaData) of
             Just foundValue ->
               return (Right foundValue)
             Nothing ->
               return dynamicNil
       Nothing ->
         return (makeEvalError ctx Nothing ("Special command 'meta' failed, can't find '" ++ show path ++ "'") Nothing)



-- | "NORMAL" COMMANDS (just like the ones in Command.hs, but these need access to 'eval', etc.)

-- | Command for loading a Carp file.
commandLoad :: CommandCallback
commandLoad [xobj@(XObj (Str path) i _)] =
  do ctx <- get
     let proj = contextProj ctx
     libDir <- liftIO $ cachePath $ projectLibDir proj
     let relativeTo = case i of
                        Just ii ->
                          case infoFile ii of
                            "REPL" -> "."
                            file -> takeDirectory file
                        Nothing -> "."
         carpDir = projectCarpDir proj
         fullSearchPaths =
           path :
           (relativeTo </> path) :                         -- the path from the file that contains the '(load)', or the current directory if not loading from a file (e.g. the repl)
           map (</> path) (projectCarpSearchPaths proj) ++ -- user defined search paths
           [carpDir </> "core" </> path] ++
           [libDir </> path]
         firstM _ [] = return Nothing
         firstM p (x:xs) = do
           q <- p x
           if q
             then return $ Just x
             else firstM p xs
     existingPath <- liftIO $ firstM doesFileExist fullSearchPaths
     case existingPath of
       Nothing ->
        if '@' `elem` path
          then tryInstall path
          else return $ invalidPath ctx path
       Just firstPathFound ->
         do canonicalPath <- liftIO (canonicalizePath firstPathFound)
            fileThatLoads <- liftIO (canonicalizePath (case i of
                                                         Just ii -> infoFile ii
                                                         Nothing -> ""))
            if canonicalPath == fileThatLoads
              then return $ cantLoadSelf ctx path
              else do let alreadyLoaded = projectAlreadyLoaded proj
                      if canonicalPath `elem` alreadyLoaded
                        then
                             return ()
                        else do contents <- liftIO $ slurp canonicalPath
                                let files = projectFiles proj
                                    files' = if canonicalPath `elem` files
                                             then files
                                             else files ++ [canonicalPath]
                                    proj' = proj { projectFiles = files', projectAlreadyLoaded = canonicalPath : alreadyLoaded }
                                newCtx <- liftIO $ executeString True (ctx { contextProj = proj' }) contents canonicalPath
                                put newCtx
                      return dynamicNil
  where
    fppl ctx =
      projectFilePathPrintLength (contextProj ctx)
    invalidPath ctx path =
      Left $ EvalError
        ((case contextExecMode ctx of
          Check ->
            machineReadableInfoFromXObj (fppl ctx) xobj ++ " I can't find a file named: '" ++ path ++ "'"
          _ -> "I can't find a file named: '" ++ path ++ "'") ++
        "\n\nIf you tried loading an external package, try appending a version string (like `@master`)") (info xobj)
    invalidPathWith ctx path stderr cleanup cleanupPath = do
      _ <- liftIO $ when cleanup (removeDirectoryRecursive cleanupPath)
      return $ Left $ EvalError
        ((case contextExecMode ctx of
          Check ->
            machineReadableInfoFromXObj (fppl ctx) xobj ++ " I can't find a file named: '" ++ path ++ "'"
          _ -> "I can't find a file named: '" ++ path ++ "'") ++
        "\n\nI tried interpreting the statement as a git import, but got: " ++ stderr) (info xobj)
    replaceC c s [] = []
    replaceC c s (a:b) = if a == c then s ++ replaceC c s b else a : replaceC c s b
    cantLoadSelf ctx path =
      case contextExecMode ctx of
        Check ->
          Left (EvalError (machineReadableInfoFromXObj (fppl ctx) xobj ++ " A file can't load itself: '" ++ path ++ "'") Nothing)
        _ ->
          Left (EvalError ("A file can't load itself: '" ++ path ++ "'") (info xobj))
    tryInstall path =
      let split = splitOn "@" path
      in tryInstallWithCheckout (joinWith "@" (init split)) (last split)
    fromURL url =
      let split = splitOn "/" (replaceC ':' "_COLON_" url)
          fst = head split
      in if fst `elem` ["https_COLON_", "http_COLON_"]
        then joinWith "/" (tail (tail split))
        else
          if '@' `elem` fst
            then joinWith "/" (joinWith "@" (tail (splitOn "@" fst)) : tail split)
            else url
    tryInstallWithCheckout path toCheckout = do
      ctx <- get
      let proj = contextProj ctx
      fpath <- liftIO $ cachePath $ projectLibDir proj </> fromURL path </> toCheckout
      cur <- liftIO getCurrentDirectory
      pathExists <- liftIO $ doesPathExist fpath
      let cleanup = not pathExists
      _ <- liftIO $ createDirectoryIfMissing True fpath
      _ <- liftIO $ setCurrentDirectory fpath
      (_, txt, _) <- liftIO $ readProcessWithExitCode "git" ["rev-parse", "--abbrev-ref=loose", "HEAD"] ""
      if txt == "HEAD\n"
      then do
        _ <- liftIO $ setCurrentDirectory cur
        doGitLoad path fpath
      else do
        _ <- liftIO $ readProcessWithExitCode "git" ["init"] ""
        _ <- liftIO $ readProcessWithExitCode "git" ["remote", "add", "origin", path] ""
        (x0, _, stderr0) <- liftIO $ readProcessWithExitCode "git" ["fetch", "--all", "--tags"] ""
        case x0 of
          ExitFailure _ -> do
            _ <- liftIO $ setCurrentDirectory cur
            invalidPathWith ctx path stderr0 cleanup fpath
          ExitSuccess -> do
            (x1, _, stderr1) <- liftIO $ readProcessWithExitCode "git" ["checkout", toCheckout] ""
            _ <- liftIO $ setCurrentDirectory cur
            case x1 of
              ExitSuccess -> doGitLoad path fpath
              ExitFailure _ -> invalidPathWith ctx path stderr1 cleanup fpath
    doGitLoad path fpath =
      let fName = last (splitOn "/" path)
          realName' = if ".git" `isSuffixOf` fName
                       then take (length fName - 4) fName
                       else fName
          realName = if ".carp" `isSuffixOf` realName'
                      then realName'
                      else realName' ++ ".carp"
          fileToLoad = fpath </> realName
          mainToLoad = fpath </> "main.carp"
      in do
        res <- commandLoad [XObj (Str fileToLoad) Nothing Nothing]
        case res of
          ret@(Right _) -> return ret
          Left _ ->  commandLoad [XObj (Str mainToLoad) Nothing Nothing]
commandLoad [x] =
  return $ Left (EvalError ("Invalid args to `load`: " ++ pretty x) (info x))


-- | Load several files in order.
loadFiles :: Context -> [FilePath] -> IO Context
loadFiles ctxStart filesToLoad = foldM folder ctxStart filesToLoad
  where folder :: Context -> FilePath -> IO Context
        folder ctx file =
          callCallbackWithArgs ctx commandLoad [XObj (Str file) Nothing Nothing]

-- | Command for reloading all files in the project (= the files that has been loaded before).
commandReload :: CommandCallback
commandReload args =
  do ctx <- get
     let paths = projectFiles (contextProj ctx)
         f :: Context -> FilePath -> IO Context
         f context filepath = do let proj = contextProj context
                                     alreadyLoaded = projectAlreadyLoaded proj
                                 if filepath `elem` alreadyLoaded
                                   then
                                        return context
                                   else do
                                           contents <- slurp filepath
                                           let proj' = proj { projectAlreadyLoaded = filepath : alreadyLoaded }
                                           executeString False (context { contextProj = proj' }) contents filepath
     newCtx <- liftIO (foldM f ctx paths)
     put newCtx
     return dynamicNil

-- | Command for expanding a form and its macros.
commandExpand :: CommandCallback
commandExpand [xobj] =
  do ctx <- get
     result <- expandAll eval (contextGlobalEnv ctx) xobj
     case result of
       Left e ->
         return (Left (removeFppl e))
       Right expanded ->
         liftIO $ do putStrLnWithColor Yellow (pretty expanded)
                     return dynamicNil
  where removeFppl (EvalError msg info fppl) = EvalError msg info

-- | This function will show the resulting C code from an expression.
-- | i.e. (Int.+ 2 3) => "_0 = 2 + 3"
commandC :: CommandCallback
commandC [xobj] =
  do ctx <- get
     let globalEnv = contextGlobalEnv ctx
         typeEnv = contextTypeEnv ctx
     result <- expandAll eval globalEnv xobj
     case result of
       Left err -> return (Left (EvalError (show err) (info xobj)))
       Right expanded ->
         case annotate typeEnv globalEnv (setFullyQualifiedSymbols typeEnv globalEnv globalEnv expanded) of
           Left err -> return (Left (EvalError (show err) (info xobj)))
           Right (annXObj, annDeps) ->
             do let cXObj = printC annXObj
                    cDeps = concatMap printC annDeps
                    c = cDeps ++ cXObj
                liftIO (putStr c)
                return dynamicNil

-- | Helper function for commandC
printC :: XObj -> String
printC xobj =
  case checkForUnresolvedSymbols xobj of
    Left e ->
      strWithColor Red (show e ++ ", can't print resulting code.\n")
    Right _ ->
      strWithColor Green (toC All (Binder emptyMeta xobj))

-- | This allows execution of calls to non-dynamic functions (defined with 'defn') to be run from the REPL
executeFunctionAsMain :: Context -> XObj -> StateT Context IO (Either EvalError XObj)
executeFunctionAsMain ctx expression =
  let fppl = projectFilePathPrintLength (contextProj ctx)
      tempMainFunction x = XObj (Lst [XObj (Defn Nothing) (Just dummyInfo) Nothing
                                     ,XObj (Sym (SymPath [] "main") Symbol) (Just dummyInfo) Nothing
                                     ,XObj (Arr []) (Just dummyInfo) Nothing
                                     ,case ty x of
                                        Just UnitTy -> x
                                        Just (RefTy _ _) -> XObj (Lst [XObj (Sym (SymPath [] "println*") Symbol) (Just dummyInfo) Nothing, x])
                                                               (Just dummyInfo) (Just UnitTy)
                                        Just _ -> XObj (Lst [XObj (Sym (SymPath [] "println*") Symbol) (Just dummyInfo) Nothing,
                                                             XObj (Lst [XObj Ref (Just dummyInfo) Nothing, x])
                                                                   (Just dummyInfo) (Just UnitTy)])
                                                       (Just dummyInfo) (Just UnitTy)
                                     ]) (Just dummyInfo) (Just (FuncTy [] UnitTy StaticLifetimeTy))
  in  do r <- annotateWithinContext False expression
         case r of
           Right (annXObj, annDeps) ->
             do let m = tempMainFunction annXObj

                ctxAfterExpansion <- get
                ctxWithDeps <- liftIO $ foldM (define True) ctxAfterExpansion annDeps
                put ctxWithDeps

                defineResult <- specialCommandDefine m
                case defineResult of
                  Left e -> return (Left e)
                  Right _ ->
                    do buildResult <- commandBuild True []
                       case buildResult of
                         Left e -> return (Left (e fppl))
                         Right _ ->
                           do executionResult <- commandRunExe []
                              case executionResult of
                                Left e -> return (Left (e fppl))
                                Right _ -> return dynamicNil
           Left err ->
             return (Left err)
