module EvalBound
  ( BoundRef (..),
    BoundExpr (..),
    refToSymPath,
    boundToXObj,
  )
where

import Info (Info)
import Obj
import SymPath
import Types (Ty)

-- | Reference target for a bound symbol.
--
-- Stage A keeps this conservative; execution still goes through the legacy
-- evaluator path.
data BoundRef
  = BoundLocalSlot Int
  | BoundInternal SymPath
  | BoundGlobal SymPath
  | BoundDynamic SymPath
  | BoundUnresolved SymPath
  deriving (Show, Eq)

-- | Stage-A bound representation of dynamic forms.
data BoundExpr
  = BoundSymbol BoundRef SymbolMode (Maybe Info) (Maybe Ty)
  | BoundList [BoundExpr] (Maybe Info) (Maybe Ty)
  | BoundArray [BoundExpr] (Maybe Info) (Maybe Ty)
  | BoundStaticArray [BoundExpr] (Maybe Info) (Maybe Ty)
  | BoundLiteral XObj
  deriving (Show, Eq)

-- | Convert a binding reference back into a symbol path (for fallback).
refToSymPath :: BoundRef -> SymPath
refToSymPath (BoundLocalSlot slot) = SymPath [] ("$slot" ++ show slot)
refToSymPath (BoundInternal path) = path
refToSymPath (BoundGlobal path) = path
refToSymPath (BoundDynamic path) = path
refToSymPath (BoundUnresolved path) = path

-- | Lower a bound expression back into an `XObj`.
--
-- Stage A executor still uses legacy evaluation, so this round-trip exists to
-- preserve behavior while we carve out architecture seams.
boundToXObj :: BoundExpr -> XObj
boundToXObj (BoundSymbol ref mode i t) = XObj (Sym (refToSymPath ref) mode) i t
boundToXObj (BoundList xs i t) = XObj (Lst (map boundToXObj xs)) i t
boundToXObj (BoundArray xs i t) = XObj (Arr (map boundToXObj xs)) i t
boundToXObj (BoundStaticArray xs i t) = XObj (StaticArr (map boundToXObj xs)) i t
boundToXObj (BoundLiteral x) = x
