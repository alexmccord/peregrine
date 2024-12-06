module Language.Assembly.ISA.X64.Insn
  ( module Language.Assembly.ISA.X64.Immediates,
    module Language.Assembly.ISA.X64.Memory,
    module Language.Assembly.ISA.X64.Registers,
    Op (..),
    REX (..),
    Opcode (..),
    ModRM (..),
    SIB (..),
    Displacement (..),
    Insn (..),
  )
where

import Language.Assembly.ISA.X64.Immediates
import Language.Assembly.ISA.X64.Memory
import Language.Assembly.ISA.X64.Registers

data Op
  = OpImm Imm
  | OpReg Reg
  | OpMem Mem

data REX = REX
  { rexW :: Bool,
    rexR :: Bool,
    rexX :: Bool,
    rexB :: Bool
  }

-- TODO
data Opcode
  = Adc

-- TODO
data ModRM = ModRM {}

-- TODO
data SIB = SIB {}

newtype Displacement = Displacement Int

data Insn = Insn
  { rex :: REX,
    opcode :: Opcode,
    modrm :: ModRM,
    sib :: SIB,
    displacement :: Displacement
  }
