{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

module Test.Shelley.Spec.Ledger.Shrinkers where

import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as S
import Shelley.Spec.Ledger.BlockChain
import Shelley.Spec.Ledger.Coin
import Shelley.Spec.Ledger.PParams
import Shelley.Spec.Ledger.Scripts
import Shelley.Spec.Ledger.Slot
import Shelley.Spec.Ledger.Tx
import Shelley.Spec.Ledger.TxData
import Shelley.Spec.Ledger.Value
import Test.QuickCheck (shrinkIntegral, shrinkList, Arbitrary, shrink)

type CVT crypto v = (CV crypto v, Arbitrary v)

shrinkBlock ::
  Block h v ->
  [Block h v]
shrinkBlock _ = []

shrinkTx ::
  forall crypto v.
  CVT crypto v =>
  Tx crypto v ->
  [Tx crypto v]
shrinkTx (Tx _b _ws _md) =
  [Tx b' _ws _md | b' <- shrinkTxBody _b]

shrinkTxBody :: (CVT crypto v) => TxBody crypto v -> [TxBody crypto v]
shrinkTxBody (TxBody is os cs ws tf tl tu md) =
  -- shrinking inputs is probably not very beneficial
  -- [ TxBody is' os cs ws tf tl tu | is' <- shrinkSet shrinkTxIn is ] ++

  -- Shrink outputs, add the differing balance of the original and new outputs
  -- to the fees in order to preserve the invariant
  -- TODO make sure this preserves the invariant
  [ TxBody is os' cs ws (tf + (vcoin $ vminus outBalance (outputBalance os'))) tl tu md
    | os' <- toList $ shrinkStrictSeq shrinkTxOut os
  ]
  where
    -- [ TxBody is os cs' ws tf tl tu | cs' <- shrinkSeq shrinkDCert cs ] ++
    -- [ TxBody is os cs ws' tf tl tu | ws' <- shrinkWdrl ws ] ++
    -- [ TxBody is os cs ws tf' tl tu | tf' <- shrinkCoin tf ] ++
    -- [ TxBody is os cs ws tf tl' tu | tl' <- shrinkSlotNo tl ] ++
    -- [ TxBody is os cs ws tf tl tu' | tu' <- shrinkUpdate tu ]
    outBalance = outputBalance os

outputBalance :: CVT crypto v => StrictSeq (TxOut crypto v) -> v
outputBalance = foldl' (\v (TxOut _ c) -> vplus v c) vzero

shrinkTxIn :: TxIn crypto v -> [TxIn crypto v]
shrinkTxIn = const []

shrinkTxOut :: (CVT crypto v) => TxOut crypto v -> [TxOut crypto v]
shrinkTxOut (TxOut addr vl) =
  TxOut addr <$> shrink vl

shrinkCoin :: Coin -> [Coin]
shrinkCoin (Coin x) = Coin <$> shrinkIntegral x

shrinkDCert :: DCert crypto -> [DCert crypto]
shrinkDCert = const []

shrinkWdrl :: Wdrl crypto -> [Wdrl crypto]
shrinkWdrl (Wdrl m) = Wdrl <$> shrinkMap shrinkRewardAcnt shrinkCoin m

shrinkRewardAcnt :: RewardAcnt crypto -> [RewardAcnt crypto]
shrinkRewardAcnt = const []

shrinkSlotNo :: SlotNo -> [SlotNo]
shrinkSlotNo (SlotNo x) = SlotNo <$> shrinkIntegral x

shrinkUpdate :: Update crypto -> [Update crypto]
shrinkUpdate = const []

shrinkWitVKey :: WitVKey crypto v kr -> [WitVKey crypto v kr]
shrinkWitVKey = const []

shrinkScriptHash :: ScriptHash crypto -> [ScriptHash crypto]
shrinkScriptHash = const []

shrinkMultiSig :: MultiSig crypto -> [MultiSig crypto]
shrinkMultiSig = const []

shrinkSet :: Ord a => (a -> [a]) -> Set a -> [Set a]
shrinkSet f = (S.fromList <$>) . shrinkList f . toList

-- TODO can this be made more efficient?
shrinkSeq :: (a -> [a]) -> Seq a -> [Seq a]
shrinkSeq f = (Seq.fromList <$>) . shrinkList f . toList

-- TODO can this be made more efficient?
shrinkStrictSeq :: (a -> [a]) -> StrictSeq a -> [StrictSeq a]
shrinkStrictSeq f = (StrictSeq.fromList <$>) . shrinkList f . toList

shrinkMap ::
  Ord k =>
  (k -> [k]) ->
  (v -> [v]) ->
  Map k v ->
  [Map k v]
shrinkMap shrinkK shrinkV =
  (M.fromList <$>) . shrinkList shrinkPair . M.toList
  where
    shrinkPair (x, y) =
      [(x', y) | x' <- shrinkK x]
        ++ [(x, y') | y' <- shrinkV y]
