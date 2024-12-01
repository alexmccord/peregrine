module Language.Assembly.ISA.X64.Memory
  ( Mem,
  )
where

import Language.Assembly.ISA.X64.Immediates
import Language.Assembly.ISA.X64.Internal
import Language.Assembly.ISA.X64.Registers

data RegOffset = RegOffset Reg | RipOffset

data Mem = Offset (Maybe RegOffset) (Maybe Reg) ScaleFactor Int
