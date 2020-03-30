{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Hashing capabilities.

module Cardano.Crypto.Hashing
  ( -- * 'AbstractHash' type supporting different hash algorithms
    AbstractHash
  , HashAlgorithm
    -- ** Hashing
  , abstractHash
  , unsafeAbstractHash
    -- ** Conversion
  , abstractHashFromDigest
  , abstractHashFromBytes
  , unsafeAbstractHashFromBytes
  , abstractHashToBytes
    -- ** Parsing and printing
  , decodeAbstractHash

   -- * Standard 'Hash' type using Blake2b 256
  , Hash
    -- ** Hashing
  , hash
  , hashDecoded
  , hashRaw
    -- ** Conversion
  , hashFromBytes
  , unsafeHashFromBytes
  , hashToBytes
    -- ** Parsing and printing
  , decodeHash
  , hashHexF
  , mediumHashF
  , shortHashF
  )
where

import Cardano.Prelude
import qualified Prelude

import Crypto.Hash (Blake2b_256, Digest, HashAlgorithm, hashDigestSize)
import qualified Crypto.Hash as Hash
import Data.Aeson
  ( FromJSON(..)
  , FromJSONKey(..)
  , FromJSONKeyFunction(..)
  , ToJSON(..)
  , ToJSONKey(..)
  )
import Data.Aeson.Types (toJSONKeyText)
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteArray.Encoding as ByteArray
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Formatting (Format, bprint, build, fitLeft, later, sformat, (%.))
import qualified Formatting.Buildable as B (Buildable(..))

import Cardano.Binary
  ( Decoded(..)
  , DecoderError(..)
  , FromCBOR(..)
  , Raw
  , ToCBOR(..)
  , serialize
  , withWordSize
  )


--------------------------------------------------------------------------------
-- AbstractHash
--------------------------------------------------------------------------------

-- | Hash wrapper with phantom type for more type-safety
--
--   Made abstract in order to support different algorithms
newtype AbstractHash algo a = AbstractHash SBS.ShortByteString
  deriving (Eq, Ord, Generic, NFData)
  deriving NoUnexpectedThunks via UseIsNormalForm SBS.ShortByteString

instance Show (AbstractHash algo a) where
  show (AbstractHash h) = BSC.unpack
                        . ByteArray.convertToBase ByteArray.Base16
                        . SBS.fromShort
                        $ h

instance HashAlgorithm algo => Read (AbstractHash algo a) where
  readsPrec _ s = case parseBase16 $ toS s of
    Left  _  -> []
    Right bs -> case abstractHashFromBytes bs of
      Nothing -> []
      Just h  -> [(h, "")]

instance B.Buildable (AbstractHash algo a) where
  build = bprint mediumHashF

instance ToJSON (AbstractHash algo a) where
  toJSON = toJSON . sformat hashHexF

instance HashAlgorithm algo => FromJSON (AbstractHash algo a) where
  parseJSON = toAesonError . readEither <=< parseJSON

instance (HashAlgorithm algo, FromJSON (AbstractHash algo a))
         => FromJSONKey (AbstractHash algo a) where
  fromJSONKey = FromJSONKeyTextParser (toAesonError . decodeAbstractHash)

instance ToJSONKey (AbstractHash algo a) where
  toJSONKey = toJSONKeyText (sformat hashHexF)

instance (Typeable algo, Typeable a, HashAlgorithm algo) => ToCBOR (AbstractHash algo a) where
  toCBOR (AbstractHash h) = toCBOR h

  encodedSizeExpr _ _ =
    let realSz = hashDigestSize (panic "unused, I hope!" :: algo)
    in fromInteger (toInteger (withWordSize realSz + realSz))

instance (Typeable algo, Typeable a, HashAlgorithm algo) => FromCBOR (AbstractHash algo a) where
  -- FIXME bad decode: it reads an arbitrary-length byte string.
  -- Better instance: know the hash algorithm up front, read exactly that
  -- many bytes, fail otherwise. Then convert to a digest.
  fromCBOR = do
    bs <- fromCBOR @ByteString
    maybe
      (cborError $ DecoderErrorCustom
        "AbstractHash"
        "Cannot convert ByteString to digest"
      )
      pure
      (abstractHashFromBytes bs)

instance HeapWords (AbstractHash algo a) where
  heapWords _
    -- We have
    --
    -- > newtype AbstractHash algo a = AbstractHash ShortByteString
    -- > data ShortByteString = SBS ByteArray#
    --
    -- so @AbstractHash algo a@ requires:
    --
    -- - 1 word for the 'ShortByteString' object header
    -- - 1 word for the pointer to the byte array object
    -- - 1 word for the byte array object header
    -- - 1 word for the size of the byte array payload in bytes
    -- - 4 words (on a 64-bit arch) for the byte array payload containing the digest
    --
    -- +---------+
    -- │ SBS │ * │
    -- +-------+-+
    --         |
    --         v
    --         +--------------+
    --         │BA#│sz│payload│
    --         +--------------+
    --
    = 8

-- | Parses given hash in base16 form.
decodeAbstractHash
  :: HashAlgorithm algo => Text -> Either Text (AbstractHash algo a)
decodeAbstractHash prettyHash = do
  bytes <- first (sformat build) $ parseBase16 prettyHash
  case abstractHashFromBytes bytes of
    Nothing -> Left
      (  "decodeAbstractHash: "
      <> "can't convert bytes to hash,"
      <> " the value was "
      <> toS prettyHash
      )
    Just h -> return h

-- | Hash the 'ToCBOR'-serialised version of a value
abstractHash :: (HashAlgorithm algo, ToCBOR a) => a -> AbstractHash algo a
abstractHash = unsafeAbstractHash . serialize

-- | Hash a lazy 'LByteString'
--
-- You can choose the phantom type, hence the \"unsafe\".
unsafeAbstractHash :: HashAlgorithm algo => LByteString -> AbstractHash algo a
unsafeAbstractHash = abstractHashFromDigest . Hash.hashlazy

-- | Make an 'AbstractHash' from a 'Digest' for the same 'HashAlgorithm'.
--
abstractHashFromDigest :: Digest algo -> AbstractHash algo a
abstractHashFromDigest = AbstractHash . SBS.toShort . ByteArray.convert

-- | Make an 'AbstractHash' from the bytes representation of the hash. It will
-- fail if given the wrong number of bytes for the choice of 'HashAlgorithm'.
--
abstractHashFromBytes :: HashAlgorithm algo => ByteString -> Maybe (AbstractHash algo a)
abstractHashFromBytes =
  fmap abstractHashFromDigest . Hash.digestFromByteString

-- | Like 'abstractHashFromDigestBytes' but the number of bytes provided
-- /must/ be correct for the choice of 'HashAlgorithm'.
--
unsafeAbstractHashFromBytes :: ByteString -> AbstractHash algo a
unsafeAbstractHashFromBytes = AbstractHash . SBS.toShort

-- | The bytes representation of the hash value.
--
abstractHashToBytes :: AbstractHash algo a -> ByteString
abstractHashToBytes (AbstractHash h) = SBS.fromShort h


--------------------------------------------------------------------------------
-- Hash
--------------------------------------------------------------------------------

-- | The type of our commonly used hash, Blake2b 256
type Hash = AbstractHash Blake2b_256

-- | The hash of a value, serialised via 'ToCBOR'.
hash :: ToCBOR a => a -> Hash a
hash = abstractHash

-- | The hash of a value's annotation
hashDecoded :: (Decoded t) => t -> Hash (BaseType t)
hashDecoded = unsafeAbstractHash . LBS.fromStrict . recoverBytes

-- | Hash a bytestring
hashRaw :: LBS.ByteString -> Hash Raw
hashRaw = unsafeAbstractHash

-- | Make a hash from it bytes representation. It must be a 32-byte bytestring.
-- The size is checked.
hashFromBytes :: ByteString -> Maybe (Hash a)
hashFromBytes = abstractHashFromBytes

-- | Make a hash from a 32-byte bytestring. It must be exactly 32 bytes.
unsafeHashFromBytes :: ByteString -> Hash a
unsafeHashFromBytes = unsafeAbstractHashFromBytes

-- | The bytes representation of the hash value.
--
hashToBytes :: AbstractHash algo a -> ByteString
hashToBytes = abstractHashToBytes

-- | Parses given hash in base16 form.
decodeHash :: Text -> Either Text (Hash a)
decodeHash = decodeAbstractHash @Blake2b_256

-- | Specialized formatter for 'Hash'.
hashHexF :: Format r (AbstractHash algo a -> r)
hashHexF = later $ \h -> B.build (show h :: Text)

-- | Smart formatter for 'Hash' to show only first @16@ characters of 'Hash'.
mediumHashF :: Format r (AbstractHash algo a -> r)
mediumHashF = fitLeft 16 %. hashHexF

-- | Smart formatter for 'Hash' to show only first @8@ characters of 'Hash'.
shortHashF :: Format r (AbstractHash algo a -> r)
shortHashF = fitLeft 8 %. hashHexF
