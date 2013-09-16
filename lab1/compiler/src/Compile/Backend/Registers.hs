module Compile.Backend.Registers where

import Compile.Types
import qualified Data.Map as Map

regMap :: Map.Map Int String 
regMap = Map.fromList [(0, "%eax"),
                      (1, "%ebx"),
                      (2, "%ecx"),
                      (3, "%edx"),
                      (4, "%esi"),
                      (5, "%edi"),
                      (6, "%ebp"),
                      (7, "%esp"),
                      (8, "%r8d"),
                      (9, "%r9d"),
                      (10, "%r10d"),
                      (11, "%r11d"),
                      (12, "%r12d"),
                      (13, "%r13d"),
                      (14, "%r14d")]
 
eax_color_num :: Int
eax_color_num = 0

edx_color_num :: Int
edx_color_num = 3

spillReg = "%r15"
