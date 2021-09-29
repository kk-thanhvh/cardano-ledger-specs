{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Ledger.BHeaderView where

import Cardano.Ledger.BaseTypes (BoundedRational (..), UnitInterval)
import Cardano.Ledger.Hashes (EraIndependentBlockBody)
import Cardano.Ledger.Keys (Hash, KeyHash, KeyRole (..))
import Cardano.Ledger.Slot (SlotNo (..), (-*))
import Numeric.Natural (Natural)

data BHeaderView crypto = BHeaderView
  { bhviewID :: KeyHash 'BlockIssuer crypto,
    bhviewBSize :: Natural,
    bhviewHSize :: Int,
    bhviewBHash :: Hash crypto EraIndependentBlockBody,
    bhviewSlot :: SlotNo
  }

isOverlaySlot ::
  SlotNo -> -- starting slot
  UnitInterval -> -- decentralization parameter
  SlotNo -> -- slot to check
  Bool
isOverlaySlot firstSlotNo dval slot = step s < step (s + 1)
  where
    s = fromIntegral $ slot -* firstSlotNo
    d = unboundRational dval
    step :: Rational -> Integer
    step x = ceiling (x * d)
