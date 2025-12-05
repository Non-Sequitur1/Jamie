module Grammar where

data Instruction = IncDataPoint
         | DecDataPoint
         | IncDataVal
         | DecDataVal
         | OutDataVal
         | AcceptOneByteIn
         | WhileDataValNonZero [Instruction] deriving (Eq, Show)

-- Grammar for the Jamie language.
