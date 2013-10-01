module Compile.Backend.Registers where

import Compile.Types
import qualified Data.Map as Map

regMap :: Map.Map Int String
regMap = Map.fromList [(0, "%eax"),
                      (1, "%ebx"),
                      (2, "%ecx"),
                      (3, "%edx"),
                      (4, "%r8d"),
                      (5, "%r9d"),
                      (6, "%r10d"),
                      (7, "%r11d"),
                      (8, "%r12d"),
                      (9, "%r13d"),
                      (10, "%r14d"),
                      (11, "%r15d")]

regByteMap :: Map.Map Int String
regByteMap = Map.fromList [(0, "%al"),
                           (1, "%bl"),
                           (2, "%cl"),
                           (3, "%dl"),
                           (4, "%r8b"),
                           (5, "%r9b"),
                           (6, "%r10b"),
                           (7, "%r11b"),
                           (8, "%r12b"),
                           (9, "%r13b"),
                           (10, "%r14b"),
                           (11, "%r15b")]

eax_color_num :: Int
eax_color_num = 0

edx_color_num :: Int
edx_color_num = 3

ecx_color_num :: Int
ecx_color_num = 2

spill_reg_num :: Int
spill_reg_num = 11

max_color_num :: Int
max_color_num = 10
