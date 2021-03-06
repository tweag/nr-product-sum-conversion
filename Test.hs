{-# LANGUAGE DataKinds, GADTs, RankNTypes, TypeOperators, KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import Prelude hiding ((<>))

import Data.Kind
import Data.Type.Equality

import GHC.Cmm.Expr
import GHC.Types.Unique

-- | A value of type `W pre post` represents WebAssembly code that
-- expects the WebAssembly evaluation stack in state `pre` and leaves
-- it in state `post`.

data W :: [Type] -> [Type] -> Type where

  Seq :: W pre mid -> W mid post -> W pre post

  I :: Int -> W stack (Int ': stack)
  F :: Float -> W stack (Float ': stack)
  Addi :: W (Int ': Int ': stack) (Int ': stack)
  LocalVar :: VarIndex -> W stack (ty ': stack)

(<>) :: forall pre mid post . W pre mid -> W mid post -> W pre post
(<>) = Seq

type VarIndex = Int
index :: Unique -> VarIndex
index = error "unimp; needs a context"



translate :: CmmExpr -> W stack (Int ': stack)
translate (CmmLit v) = I (asInt v)
translate (CmmMachOp (MO_Add _) [e1, e2]) =
    translate e1 <> translate e2 <> Addi
translate _ = error "not impelmented"


asInt :: CmmLit -> Int
asInt (CmmInt n _) = fromIntegral n
asInt _ = error "not an integer literal"

----------------------------------------------------------------

as32 :: String -> Width -> a -> a
as32 _ W32 = id
as32 what w = error $ what ++ " width " ++ show w ++ " is not supported in wasm"

-- codes for, roughly: `CmmLit -> exists t . W stack (t ': stack)`
lit :: CmmLit -> (forall t . WTy t -> W stack (t ': stack) -> answer) -> answer
lit (CmmInt   n w) k = as32 "integer" w $ k WInt $ I $ fromIntegral n
lit (CmmFloat r w) k = as32 "float"   w $ k WFloat $ F $ fromRational r
lit _ _ = error "unimp"

typeError :: String -> CmmExpr -> a
typeError _ _ = error "type error in Cmm expression"

expr :: forall stack answer .
        CmmExpr -> (forall t . WTy t -> W stack (t ': stack) -> answer) -> answer
expr (CmmLit v) k = lit v k
expr (CmmReg (CmmLocal (LocalReg x ty))) k =
  as32 "local register" (typeWidth ty) $
  if isFloatType ty then
      k WFloat $ LocalVar (index x)
  else
      k WInt $ LocalVar (index x)
expr (CmmMachOp (MO_Add w) [e1, e2]) k =
    as32 "add" w $
    iexpr e1 $ \w1 ->
        iexpr e2 $ \w2 ->
            k WInt $ w1 <> w2 <> Addi

expr _ _ = error "unimp"

iexpr :: forall stack answer . CmmExpr -> (W stack (Int ': stack) -> answer) -> answer
iexpr e k = expr e k'
  where k' :: forall t . WTy t -> W stack (t ': stack) -> answer
        k' WInt w = k w
        k' _ _ = typeError "expected integer expression but got" e


-- a Wasm value type
data Ty = TInt
        | TFloat
  deriving (Show, Eq)

-- | The singleton for a Wasm value type
data WTy :: Type -> Type where
  WInt   :: WTy Int
  WFloat  :: WTy Float

deriving instance Show (WTy ty)


instance TestEquality WTy where
  testEquality WInt WInt     = Just Refl
  testEquality WFloat WFloat = Just Refl
  testEquality _ _ = Nothing


oldlit :: CmmLit -> (forall pre post . W pre post -> answer) -> answer
oldlit (CmmInt   n w) k = as32 "integer" w $ k $ I $ fromIntegral n
oldlit (CmmFloat r w) k = as32 "float"   w $ k $ F $ fromRational r
oldlit _ _ = error "unimp"

badcps :: CmmExpr -> (forall pre post . W pre post -> answer) -> answer
badcps (CmmLit v) k = oldlit v k
badcps (CmmReg (CmmLocal (LocalReg x ty))) k =
  as32 "local register" (typeWidth ty) $
  if isFloatType ty then
      k $ (LocalVar (index x) :: forall stack . W stack (Float ': stack))
  else
      k $ (LocalVar (index x) :: forall stack . W stack (Int ': stack))
badcps (CmmMachOp (MO_Add w) [e1, e2]) k =
    as32 "add" w $
    badcps e1 $ \w1 -> badcps e2 $ \w2 -> k $ undefined w1 <> undefined w2 <> Addi
badcps _ _ = error "unimp"
