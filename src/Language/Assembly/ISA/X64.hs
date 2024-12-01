module Language.Assembly.ISA.X64
  ( module Language.Assembly.ISA.X64.Immediates,
    module Language.Assembly.ISA.X64.Registers,
    module Language.Assembly.ISA.X64.Memory,
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

data Opcode
  = Adc

data ModRM = ModRM {}

data SIB = SIB {}

newtype Displacement = Displacement Int

data Insn = Insn
  { rex :: REX,
    opcode :: Opcode,
    modrm :: ModRM,
    sib :: SIB,
    displacement :: Displacement
  }

-- mkProgram :: [X86]
-- mkProgram =
--   [ Push (OpReg $ Reg RS64 StackBasePointer),
--     Mov (OpReg $ Reg RS64 StackBasePointer) (OpReg $ Reg RS64 StackPointer),
--     Mov (OpMem $ Offset (Just $ RegOffset (Reg RS64 StackBasePointer)) Nothing S1 (-4)) (OpImm $ ImmInt 5),
--     Mov (OpReg $ Reg RS32 Accumulator) (OpMem $ Offset (Just $ RegOffset (Reg RS64 StackBasePointer)) Nothing S1 (-4)),
--     Add (OpReg $ Reg RS32 Accumulator) (OpImm $ ImmInt 5),
--     Pop (OpReg $ Reg RS64 Accumulator),
--     Ret
--   ]
