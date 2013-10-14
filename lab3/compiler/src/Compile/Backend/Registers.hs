module Compile.Backend.Registers where

import Compile.Types
import qualified Data.Map as Map

regMap :: Map.Map Int String
regMap = Map.fromList [(0, "%eax"),
                      (1, "%ecx"),
                      (2, "%edx"),
                      (3, "%r8d"),
                      (4, "%r9d"),
                      (5, "%r10d"),
                      (6, "%r11d"),
                      (7, "%ebx"),
                      (8, "%r12d"),
                      (9, "%r13d"),
                      (10, "%r14d"),
                      (11, "%r15d"),
                      (12, "%edi"),
                      (13, "%esi")]

regQMap :: Map.Map Int String
regQMap = Map.fromList [(0, "%rax"),
                       (1, "%rcx"),
                       (2, "%rdx"),
                       (3, "%r8"),
                       (4, "%r9"),
                       (5, "%r10"),
                       (6, "%r11"),
                       (7, "%rbx"),
                       (8, "%r12"),
                       (9, "%r13"),
                       (10, "%r14"),
                       (11, "%r15"),
                       (12, "%rdi"),
                       (13, "%rsi")]

callers = ["%rax", "%rdx", "%rcx", "%r8", "%r9", "%r10", "%r11"]
callees = ["%rbx", "%r12", "%r13", "%r14"]

argArr :: [Int]
argArr = [12, 13, 2, 1, 3, 4]

regByteMap :: Map.Map Int String
regByteMap = Map.fromList [(0, "%al"),
                           (1, "%cl"),
                           (2, "%dl"),
                           (3, "%r8b"),
                           (4, "%r9b"),
                           (5, "%r10b"),
                           (6, "%r11b"),
                           (7, "%bl"),
                           (8, "%r12b"),
                           (9, "%r13b"),
                           (10, "%r14b"),
                           (11, "%r15b")]

eax_color_num :: Int
eax_color_num = 0

edx_color_num :: Int
edx_color_num = 2

ecx_color_num :: Int
ecx_color_num = 1

spill_reg_num :: Int
spill_reg_num = 11

max_color_num :: Int
max_color_num = 10
