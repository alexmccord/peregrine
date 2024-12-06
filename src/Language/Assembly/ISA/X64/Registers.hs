module Language.Assembly.ISA.X64.Registers
  ( Reg,
    -- 64 bit registers
    pattern RAX,
    pattern RCX,
    pattern RDX,
    pattern RBX,
    pattern RSP,
    pattern RBP,
    pattern RSI,
    pattern RDI,
    pattern R8,
    pattern R9,
    pattern R10,
    pattern R11,
    pattern R12,
    pattern R13,
    pattern R14,
    pattern R15,
    -- 32 bit registers
    pattern EAX,
    pattern ECX,
    pattern EDX,
    pattern EBX,
    pattern ESP,
    pattern EBP,
    pattern ESI,
    pattern EDI,
    pattern R8D,
    pattern R9D,
    pattern R10D,
    pattern R11D,
    pattern R12D,
    pattern R13D,
    pattern R14D,
    pattern R15D,
    -- 16 bit registers
    pattern AX,
    pattern CX,
    pattern DX,
    pattern BX,
    pattern SP,
    pattern BP,
    pattern SI,
    pattern DI,
    pattern R8W,
    pattern R9W,
    pattern R10W,
    pattern R11W,
    pattern R12W,
    pattern R13W,
    pattern R14W,
    pattern R15W,
    -- 8 bit registers
    pattern AL,
    pattern CL,
    pattern DL,
    pattern BL,
    pattern SPL,
    pattern BPL,
    pattern SIL,
    pattern DIL,
    pattern R8B,
    pattern R9B,
    pattern R10B,
    pattern R11B,
    pattern R12B,
    pattern R13B,
    pattern R14B,
    pattern R15B,
  )
where

import Data.Bitset
import GHC.TypeLits

data Legacy = LS32 | LS16 | LS8L

data Extended = ES64 | ES32 | ES16 | ES8

pattern RegBitset :: (KnownNat n) => Int -> Bitset n
pattern RegBitset n <- (bitsetToIntegral -> n)
  where
    RegBitset = bitsetFromIntegral

data Reg
  = Legacy Legacy (Bitset 3)
  | Extended Extended (Bitset 4)

pattern RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15 :: Reg
pattern RAX = Extended ES64 (RegBitset 0)
pattern RCX = Extended ES64 (RegBitset 1)
pattern RDX = Extended ES64 (RegBitset 2)
pattern RBX = Extended ES64 (RegBitset 3)
pattern RSP = Extended ES64 (RegBitset 4)
pattern RBP = Extended ES64 (RegBitset 5)
pattern RSI = Extended ES64 (RegBitset 6)
pattern RDI = Extended ES64 (RegBitset 7)
pattern R8 = Extended ES64 (RegBitset 8)
pattern R9 = Extended ES64 (RegBitset 9)
pattern R10 = Extended ES64 (RegBitset 10)
pattern R11 = Extended ES64 (RegBitset 11)
pattern R12 = Extended ES64 (RegBitset 12)
pattern R13 = Extended ES64 (RegBitset 13)
pattern R14 = Extended ES64 (RegBitset 14)
pattern R15 = Extended ES64 (RegBitset 15)

pattern EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D :: Reg
pattern EAX = Legacy LS32 (RegBitset 0)
pattern ECX = Legacy LS32 (RegBitset 1)
pattern EDX = Legacy LS32 (RegBitset 2)
pattern EBX = Legacy LS32 (RegBitset 3)
pattern ESP = Legacy LS32 (RegBitset 4)
pattern EBP = Legacy LS32 (RegBitset 5)
pattern ESI = Legacy LS32 (RegBitset 6)
pattern EDI = Legacy LS32 (RegBitset 7)
pattern R8D = Extended ES32 (RegBitset 8)
pattern R9D = Extended ES32 (RegBitset 9)
pattern R10D = Extended ES32 (RegBitset 10)
pattern R11D = Extended ES32 (RegBitset 11)
pattern R12D = Extended ES32 (RegBitset 12)
pattern R13D = Extended ES32 (RegBitset 13)
pattern R14D = Extended ES32 (RegBitset 14)
pattern R15D = Extended ES32 (RegBitset 15)

pattern AX, CX, DX, BX, SP, BP, SI, DI, R8W, R9W, R10W, R11W, R12W, R13W, R14W, R15W :: Reg
pattern AX = Legacy LS16 (RegBitset 0)
pattern CX = Legacy LS16 (RegBitset 1)
pattern DX = Legacy LS16 (RegBitset 2)
pattern BX = Legacy LS16 (RegBitset 3)
pattern SP = Legacy LS16 (RegBitset 4)
pattern BP = Legacy LS16 (RegBitset 5)
pattern SI = Legacy LS16 (RegBitset 6)
pattern DI = Legacy LS16 (RegBitset 7)
pattern R8W = Extended ES16 (RegBitset 8)
pattern R9W = Extended ES16 (RegBitset 9)
pattern R10W = Extended ES16 (RegBitset 10)
pattern R11W = Extended ES16 (RegBitset 11)
pattern R12W = Extended ES16 (RegBitset 12)
pattern R13W = Extended ES16 (RegBitset 13)
pattern R14W = Extended ES16 (RegBitset 14)
pattern R15W = Extended ES16 (RegBitset 15)

pattern AL, CL, DL, BL, SPL, BPL, SIL, DIL, R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B :: Reg
pattern AL = Legacy LS8L (RegBitset 0)
pattern CL = Legacy LS8L (RegBitset 1)
pattern DL = Legacy LS8L (RegBitset 2)
pattern BL = Legacy LS8L (RegBitset 3)
pattern SPL = Legacy LS8L (RegBitset 4)
pattern BPL = Legacy LS8L (RegBitset 5)
pattern SIL = Legacy LS8L (RegBitset 6)
pattern DIL = Legacy LS8L (RegBitset 7)
pattern R8B = Extended ES8 (RegBitset 8)
pattern R9B = Extended ES8 (RegBitset 9)
pattern R10B = Extended ES8 (RegBitset 10)
pattern R11B = Extended ES8 (RegBitset 11)
pattern R12B = Extended ES8 (RegBitset 12)
pattern R13B = Extended ES8 (RegBitset 13)
pattern R14B = Extended ES8 (RegBitset 14)
pattern R15B = Extended ES8 (RegBitset 15)
