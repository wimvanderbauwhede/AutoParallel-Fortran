module Platform (Platform(..)) where

-- the -plat flag takes these arguments, in uppercase
data Platform = CPU | MIC | GPU | FPGA deriving (Eq,Show,Read)  
