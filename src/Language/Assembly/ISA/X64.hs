module Language.Assembly.ISA.X64 () where

-- import Language.Assembly.ISA.X64.Insn

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
