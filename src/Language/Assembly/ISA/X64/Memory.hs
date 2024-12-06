module Language.Assembly.ISA.X64.Memory
  ( Mem (..),
    RegOffset (..),
    ScaleFactor (..),
  )
where

-- import Language.Assembly.ISA.X64.Immediates

import Data.Char
import Language.Assembly.ISA.X64.Registers

newtype ScaleFactor = S Int

data RegOffset = RegOffset Reg | RipOffset

data Mem = Offset (Maybe RegOffset) (Maybe Reg) ScaleFactor Int
