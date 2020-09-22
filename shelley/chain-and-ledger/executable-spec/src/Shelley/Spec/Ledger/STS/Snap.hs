{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Shelley.Spec.Ledger.STS.Snap
  ( SNAP,
    PredicateFailure,
  )
where

import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era)
import qualified Cardano.Ledger.Val as Val
import Cardano.Prelude (NoUnexpectedThunks (..))
import Control.State.Transition
  ( STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
  )
import GHC.Generics (Generic)
import Shelley.Spec.Ledger.BaseTypes
import Shelley.Spec.Ledger.EpochBoundary
import Shelley.Spec.Ledger.LedgerState
  ( DPState (..),
    LedgerState (..),
    UTxOState (..),
    stakeDistr,
  )

data SNAP era

data SnapPredicateFailure era -- No predicate failures
  deriving (Show, Generic, Eq)

instance NoUnexpectedThunks (SnapPredicateFailure era)

instance (Era era, Core.ValType era, (Val.Val (Core.Value era))) => STS (SNAP era) where
  type State (SNAP era) = SnapShots era
  type Signal (SNAP era) = ()
  type Environment (SNAP era) = LedgerState era
  type BaseM (SNAP era) = ShelleyBase
  type PredicateFailure (SNAP era) = SnapPredicateFailure era
  initialRules = [pure emptySnapShots]
  transitionRules = [snapTransition]

snapTransition ::
  ( Era era,
    Core.ValType era,
    (Val.Val (Core.Value era))
  ) =>
  TransitionRule (SNAP era)
snapTransition = do
  TRC (lstate, s, _) <- judgmentContext

  let LedgerState (UTxOState utxo _ fees _) (DPState dstate pstate) = lstate
      stake = stakeDistr utxo dstate pstate
  pure $
    s
      { _pstakeMark = stake,
        _pstakeSet = _pstakeMark s,
        _pstakeGo = _pstakeSet s,
        _feeSS = fees
      }
