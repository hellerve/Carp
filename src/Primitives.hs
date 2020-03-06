module Primitives where

import Control.Monad.State.Lazy (StateT(..), get, put, liftIO, foldM, when, unless)
import Data.List (foldl')
import qualified Data.Map as Map

import ColorText
import Commands
import Deftype
import Emit
import Lookup
import Obj
import TypeError
import Types
import Util

type Primitive = XObj -> Env -> [XObj] -> StateT Context IO (Either EvalError XObj)

found binder =
  liftIO $ do putStrLnWithColor White (show binder)
              return dynamicNil

makePrim :: String -> Int -> String -> Primitive -> (SymPath, Primitive)
makePrim name arity example callback =
  makePrim' name (Just arity) example callback

makeVarPrim :: String -> String -> Primitive -> (SymPath, Primitive)
makeVarPrim name example callback =
  makePrim' name Nothing example callback

makePrim' :: String -> Maybe Int -> String -> Primitive -> (SymPath, Primitive)
makePrim' name maybeArity example callback =
  let path = SymPath [] name
  in (path, wrapped)
  where wrapped =
          case maybeArity of
            Just a ->
              \x e l ->
                let ll = length l
                in (if ll /= a then err x a ll else callback x e l)
            Nothing -> callback
        err :: XObj -> Int -> Int -> StateT Context IO (Either EvalError XObj)
        err x a l = do
          ctx <- get
          return (evalError ctx (
            "The primitive `" ++ name ++ "` expected " ++ show a ++
            " arguments, but got " ++ show l ++ ".\n\nExample Usage:\n```\n" ++
            example ++ "\n```\n") (info x))

primitiveFile :: Primitive
primitiveFile x@(XObj _ i t) _ [XObj _ mi _] = do
  ctx <- get
  case mi of
    Just info -> return (Right (XObj (Str (infoFile info)) i t))
    Nothing ->
      return (evalError ctx ("No information about object " ++ pretty x) (info x))

primitiveLine :: Primitive
primitiveLine x@(XObj _ i t) _ [XObj _ mi _] = do
  ctx <- get
  case mi of
    Just info -> return (Right (XObj (Num IntTy (fromIntegral (infoLine info))) i t))
    Nothing ->
      return (evalError ctx ("No information about object " ++ pretty x) (info x))

primitiveColumn :: Primitive
primitiveColumn x@(XObj _ i t) _ [XObj _ mi _] = do
  ctx <- get
  case mi of
    Just info -> return (Right (XObj (Num IntTy (fromIntegral (infoColumn info))) i t))
    Nothing ->
      return (evalError ctx ("No information about object " ++ pretty x) (info x))

existingMeta :: Env -> XObj -> MetaData
existingMeta globalEnv xobj =
  case lookupInEnv (getPath xobj) globalEnv of
    Just (_, Binder meta _) -> meta
    Nothing -> emptyMeta

registerInInterfaceIfNeeded :: Context -> SymPath -> Ty -> Either String Context
registerInInterfaceIfNeeded ctx path@(SymPath _ name) definitionSignature =
  let typeEnv = getTypeEnv (contextTypeEnv ctx)
  in case lookupInEnv (SymPath [] name) typeEnv of
       Just (_, Binder _ (XObj (Lst [XObj (Interface interfaceSignature paths) ii it, isym]) i t)) ->
         if areUnifiable interfaceSignature definitionSignature
         then let updatedInterface = XObj (Lst [XObj (Interface interfaceSignature (addIfNotPresent path paths)) ii it, isym]) i t
              in  return $ ctx { contextTypeEnv = TypeEnv (extendEnv typeEnv name updatedInterface) }
         else Left ("[INTERFACE ERROR] " ++ show path ++ " : " ++ show definitionSignature ++
                    " doesn't match the interface signature " ++ show interfaceSignature)
       Just (_, Binder _ x) ->
         error ("A non-interface named '" ++ name ++ "' was found in the type environment: " ++ show x)
       Nothing -> return ctx

-- | Ensure that a 'def' / 'defn' has registered with an interface (if they share the same name).
registerDefnOrDefInInterfaceIfNeeded :: Context -> XObj -> Either String Context
registerDefnOrDefInInterfaceIfNeeded ctx xobj =
  case xobj of
    XObj (Lst [XObj (Defn _) _ _, XObj (Sym path _) _ _, _, _]) _ (Just t) ->
      -- This is a function, does it belong to an interface?
      registerInInterfaceIfNeeded ctx path t
    XObj (Lst [XObj Def _ _, XObj (Sym path _) _ _, _]) _ (Just t) ->
      -- Global variables can also be part of an interface
      registerInInterfaceIfNeeded ctx path t
    _ -> return ctx

define :: Bool -> Context -> XObj -> IO Context
define hidden ctx@(Context globalEnv typeEnv _ proj _ _ _) annXObj =
  let previousType =
        case lookupInEnv (getPath annXObj) globalEnv of
          Just (_, Binder _ found) -> ty found
          Nothing -> Nothing
      previousMeta = existingMeta globalEnv annXObj
      adjustedMeta = if hidden
                     then previousMeta { getMeta = Map.insert "hidden" trueXObj (getMeta previousMeta) }
                     else previousMeta
      fppl = projectFilePathPrintLength proj
  in case annXObj of
       XObj (Lst (XObj (Defalias _) _ _ : _)) _ _ ->
         return (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       XObj (Lst (XObj (Deftype _) _ _ : _)) _ _ ->
         return (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       XObj (Lst (XObj (DefSumtype _) _ _ : _)) _ _ ->
         return (ctx { contextTypeEnv = TypeEnv (envInsertAt (getTypeEnv typeEnv) (getPath annXObj) (Binder adjustedMeta annXObj)) })
       _ ->
         do when (projectEchoC proj) $
              putStrLn (toC All (Binder emptyMeta annXObj))
            case previousType of
              Just previousTypeUnwrapped ->
                unless (areUnifiable (forceTy annXObj) previousTypeUnwrapped) $
                  do putStrWithColor Blue ("[WARNING] Definition at " ++ prettyInfoFromXObj annXObj ++ " changed type of '" ++ show (getPath annXObj) ++
                                           "' from " ++ show previousTypeUnwrapped ++ " to " ++ show (forceTy annXObj))
                     putStrLnWithColor White "" -- To restore color for sure.
              Nothing -> return ()
            case registerDefnOrDefInInterfaceIfNeeded ctx annXObj of
              Left err ->
                do case contextExecMode ctx of
                     Check -> let fppl = projectFilePathPrintLength (contextProj ctx)
                              in  putStrLn (machineReadableInfoFromXObj fppl annXObj ++ " " ++ err)
                     _ -> putStrLnWithColor Red err
                   return ctx
              Right ctx' ->
                return (ctx' { contextGlobalEnv = envInsertAt globalEnv (getPath annXObj) (Binder adjustedMeta annXObj) })

primitiveRegisterType :: Primitive
primitiveRegisterType _ e [XObj (Sym (SymPath [] t) _) _ _] = do
  ctx <- get
  let pathStrings = contextPath ctx
      typeEnv = contextTypeEnv ctx
      path = SymPath pathStrings t
      typeDefinition = XObj (Lst [XObj ExternalType Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
  put (ctx { contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) t typeDefinition) })
  return dynamicNil
primitiveRegisterType _ _ [x] = do
  ctx <- get
  return (evalError ctx ("`register-type` takes a symbol, but it got " ++ pretty x) (info x))
primitiveRegisterType _ _ (x@(XObj (Sym (SymPath [] t) _) _ _):members) = do
  ctx <- get
  let pathStrings = contextPath ctx
      globalEnv = contextGlobalEnv ctx
      typeEnv = contextTypeEnv ctx
      path = SymPath pathStrings t
      preExistingModule = case lookupInEnv (SymPath pathStrings t) globalEnv of
                            Just (_, Binder _ (XObj (Mod found) _ _)) -> Just found
                            _ -> Nothing
  case bindingsForRegisteredType typeEnv globalEnv pathStrings t members Nothing preExistingModule of
    Left err -> return (makeEvalError ctx (Just err) (show err) (info x))
    Right (typeModuleName, typeModuleXObj, deps) -> do
      let typeDefinition = XObj (Lst [XObj ExternalType Nothing Nothing, XObj (Sym path Symbol) Nothing Nothing]) Nothing (Just TypeTy)
          ctx' = (ctx { contextGlobalEnv = envInsertAt globalEnv (SymPath pathStrings typeModuleName) (Binder emptyMeta typeModuleXObj)
                      , contextTypeEnv = TypeEnv (extendEnv (getTypeEnv typeEnv) t typeDefinition)
                      })
      contextWithDefs <- liftIO $ foldM (define True) ctx' deps
      put contextWithDefs
      return dynamicNil

notFound :: XObj -> SymPath -> StateT Context IO (Either EvalError XObj)
notFound x path = do
  ctx <- get
  return (evalError ctx ("I can’t find the symbol `" ++ show path ++ "`") (info x))

primitiveInfo :: Primitive
primitiveInfo _ env [target@(XObj (Sym path@(SymPath _ name) _) _ _)] = do
  ctx <- get
  let typeEnv = contextTypeEnv ctx
  case path of
    SymPath [] _ ->
      -- First look in the type env, then in the global env:
      case lookupInEnv path (getTypeEnv typeEnv) of
        Nothing -> printer True True (lookupInEnv path env)
        found -> do printer True True found -- this will print the interface itself
                    printer True False (lookupInEnv path env)-- this will print the locations of the implementers of the interface
    qualifiedPath ->
      case lookupInEnv path env of
        Nothing -> notFound target path
        found -> printer False True found
  where printer allowLookupInALL errNotFound binderPair = do
          ctx <- get
          let proj = contextProj ctx
          case binderPair of
           Just (_, binder@(Binder metaData x@(XObj _ (Just i) _))) ->
             do liftIO $ putStrLn (show binder ++ "\nDefined at " ++ prettyInfo i)
                case Map.lookup "doc" (getMeta metaData) of
                  Just (XObj (Str val) _ _) -> liftIO $ putStrLn ("Documentation: " ++ val)
                  Nothing -> return ()
                liftIO $ when (projectPrintTypedAST proj) $ putStrLnWithColor Yellow (prettyTyped x)
                return dynamicNil
           Just (_, binder@(Binder metaData x)) ->
             do liftIO $ print binder
                case Map.lookup "doc" (getMeta metaData) of
                  Just (XObj (Str val) _ _) -> liftIO $ putStrLn ("Documentation: " ++ val)
                  Nothing -> return ()
                liftIO $ when (projectPrintTypedAST proj) $ putStrLnWithColor Yellow (prettyTyped x)
                return dynamicNil
           Nothing ->
             if allowLookupInALL
             then case multiLookupALL name env of
                   [] -> if errNotFound then notFound target path else return dynamicNil
                   binders -> do
                     liftIO$ mapM_ (\(env, binder@(Binder _ (XObj _ i _))) ->
                              case i of
                                Just i' -> putStrLnWithColor White (show binder ++ " Defined at " ++ prettyInfo i')
                                Nothing -> putStrLnWithColor White (show binder))
                           binders
                     return dynamicNil
            else if errNotFound then notFound target path else return dynamicNil

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

primitiveDefndynamic :: Primitive
primitiveDefndynamic _ _ [XObj (Sym (SymPath [] name) _) _ _, params, body] =
  dynamicOrMacro Dynamic DynamicTy name params body
primitiveDefndynamic _ _ [notName, params, body] = do
  ctx <- get
  return (evalError ctx ("`defndynamic` expected a name as first argument, but got " ++ pretty notName) (info notName))

primitiveDefmacro :: Primitive
primitiveDefmacro _ _ [XObj (Sym (SymPath [] name) _) _ _, params, body] =
  dynamicOrMacro Macro MacroTy name params body
primitiveDefmacro _ _ [notName, params, body] = do
  ctx <- get
  return (evalError ctx ("`defmacro` expected a name as first argument, but got " ++ pretty notName) (info notName))

primitiveType :: Primitive
primitiveType _ _ [x@(XObj (Sym path@(SymPath [] name) _) _ _)] = do
  ctx <- get
  let env = contextGlobalEnv ctx
  case lookupInEnv path env of
    Just (_, binder) ->
      found binder
    Nothing ->
      case multiLookupALL name env of
        [] ->
          notFound x path
        binders ->
          liftIO $ do mapM_ (\(env, binder) -> putStrLnWithColor White (show binder)) binders
                      return dynamicNil
primitiveType _ _ [x@(XObj (Sym qualifiedPath _) _ _)] = do
  ctx <- get
  let env = contextGlobalEnv ctx
  case lookupInEnv qualifiedPath env of
    Just (_, binder) ->
      found binder
    Nothing ->
      notFound x qualifiedPath
primitiveType _ _ [x] = do
  ctx <- get
  return (evalError ctx ("Can't get the type of non-symbol: " ++ pretty x) (info x))

primitiveMembers :: Primitive
primitiveMembers _ env [target] = do
  ctx <- get
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
               return (evalError ctx ("Can't find a struct type named '" ++ name ++ "' in type environment") (info target))
        _ -> return (evalError ctx ("Can't get the members of non-symbol: " ++ pretty target) (info target))
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

-- | Set meta data for a Binder
primitiveMetaSet :: Primitive
primitiveMetaSet _ env [target@(XObj (Sym path@(SymPath _ name) _) _ _), XObj (Str key) _ _, value] =
  do ctx <- get
     let pathStrings = contextPath ctx
         fppl = projectFilePathPrintLength (contextProj ctx)
     case lookupInEnv (consPath pathStrings path) env of
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
             return (evalError ctx ("`meta-set!` failed, I can't find the symbol `" ++ show path ++ "`") (info target))
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
primitiveMetaSet _ _ [(XObj (Sym _ _) _ _), key, _] = do
  ctx <- get
  return (evalError ctx ("`meta-set!` expected a string as its second argument, but got `" ++ pretty key ++ "`") (info key))
primitiveMetaSet _ _ [target, _, _] = do
  ctx <- get
  return (evalError ctx ("`meta-set!` expected a symbol as its first argument, but got `" ++ pretty target ++ "`") (info target))

registerInterfaceFunctions :: String -> Ty -> StateT Context IO ()
registerInterfaceFunctions name t = do
  ctx <- get
  let env = contextGlobalEnv ctx
      found = multiLookupALL name env
      binders = map snd found
      resultCtx = foldl' (\maybeCtx binder -> case maybeCtx of
                                                Right ok -> registerDefnOrDefInInterfaceIfNeeded ok (binderXObj binder)
                                                Left err -> Left err)
                         (Right ctx) binders
  case resultCtx of
    Left err -> error err
    Right ctx' -> put ctx'
  return ()

primitiveDefinterface :: Primitive
primitiveDefinterface xobj _ [nameXObj@(XObj (Sym path@(SymPath [] name) _) _ _), ty] = do
  ctx <- get
  let fppl = projectFilePathPrintLength (contextProj ctx)
      typeEnv = getTypeEnv (contextTypeEnv ctx)
  case xobjToTy ty of
    Just t ->
      case lookupInEnv path typeEnv of
        Just (_, Binder _ (XObj (Lst (XObj (Interface foundType _) _ _ : _)) _ _)) ->
          -- The interface already exists, so it will be left as-is.
          if foundType == t
          then return dynamicNil
          else return (evalError ctx ("Tried to change the type of interface `" ++ show path ++ "` from `" ++ show foundType ++ "` to `" ++ show t ++ "`") (info xobj))
        Nothing ->
          let interface = defineInterface name t [] (info nameXObj)
              typeEnv' = TypeEnv (envInsertAt typeEnv (SymPath [] name) (Binder emptyMeta interface))
          in  do put (ctx { contextTypeEnv = typeEnv' })
                 registerInterfaceFunctions name t
                 return dynamicNil
    Nothing ->
      return (evalError ctx ("Invalid type for interface `" ++ name ++ "`: " ++ pretty ty) (info ty))
primitiveDefinterface _ _ [name, _] = do
  ctx <- get
  return (evalError ctx ("`definterface` expects a name as first argument, but got `" ++ pretty name ++ "`") (info name))