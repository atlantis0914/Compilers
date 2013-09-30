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
                      (6, "%r8d"),
                      (7, "%r9d"),
                      (8, "%r10d"),
                      (9, "%r11d"),
                      (10, "%r12d"),
                      (11, "%r13d"),
                      (12, "%r14d"),
                      (13, "%r15d")]

regByteMap :: Map.Map Int String
regByteMap = Map.fromList [(0, "%al"),
                           (1, "%bl"),
                           (2, "%cl"),
                           (3, "%dl"),
                           (4, "%esi"),
                           (5, "%edi"),
                           (6, "%r8d"),
                           (7, "%r9d"),
                           (8, "%r10d"),
                           (9, "%r11d"),
                           (10, "%r12d"),
                           (11, "%r13d"),
                           (12, "%r14d"),
                           (13, "%r15d")]

eax_color_num :: Int
eax_color_num = 0

edx_color_num :: Int
edx_color_num = 3

spill_reg_num :: Int
spill_reg_num = 13

max_color_num :: Int
max_color_num = 12
