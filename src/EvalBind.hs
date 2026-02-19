module EvalBind
  ( bindExpr,
    bindProgram,
  )
where

import qualified Env as E
import EvalBound
import Obj
import qualified Set
import SymPath

bindProgram :: Context -> [XObj] -> [BoundExpr]
bindProgram ctx = map (bindExpr ctx)

-- | Stage-A binder: classify symbols to stable-ish reference categories.
--
-- No behavior changes yet; this pass is used only to establish a binding seam.
bindExpr :: Context -> XObj -> BoundExpr
bindExpr ctx xobj@(XObj obj i t) =
  case obj of
    Sym spath mode -> BoundSymbol (classifySymbol ctx spath) mode i t
    Lst xs -> BoundList (map (bindExpr ctx) xs) i t
    Arr xs -> BoundArray (map (bindExpr ctx) xs) i t
    StaticArr xs -> BoundStaticArray (map (bindExpr ctx) xs) i t
    _ -> BoundLiteral xobj

classifySymbol :: Context -> SymPath -> BoundRef
classifySymbol ctx spath@(SymPath p n) =
  case contextInternalEnv ctx of
    Just internal
      | null p && isFound (E.searchBinder internal spath) ->
        BoundInternal spath
    _ -> classifyGlobal
  where
    global = contextGlobalEnv ctx
    dynamicPath = SymPath ("Dynamic" : p) n
    contextualPath = SymPath (contextPath ctx ++ p) n
    inUsePath = findInUseModulePath global p n
    classifyGlobal
      | isFound (E.searchBinder global dynamicPath) = BoundDynamic dynamicPath
      | isFound (E.searchBinder global spath) = BoundGlobal spath
      | isFound (E.searchBinder global contextualPath) = BoundGlobal contextualPath
      | otherwise =
        case inUsePath of
          Just path -> BoundGlobal path
          Nothing -> BoundUnresolved spath

findInUseModulePath :: Env -> [String] -> String -> Maybe SymPath
findInUseModulePath global p n =
  let useCandidates =
        [ SymPath (p' ++ (n' : p)) n
          | SymPath p' n' <- Set.toList (envUseModules global)
        ]
   in findFirst (\candidate -> isFound (E.searchBinder global candidate)) useCandidates

findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst _ [] = Nothing
findFirst predicate (x : xs)
  | predicate x = Just x
  | otherwise = findFirst predicate xs

isFound :: Either a b -> Bool
isFound (Right _) = True
isFound _ = False
