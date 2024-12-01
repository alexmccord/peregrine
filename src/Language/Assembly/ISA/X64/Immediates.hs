module Language.Assembly.ISA.X64.Immediates
  ( Imm,
  )
where

data Imm
  = ImmInt Int
  | ImmFloat Float
