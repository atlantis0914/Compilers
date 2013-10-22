module Compile.Backend.GenAsm where

import Compile.Types
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

import Compile.Backend.Registers
import Compile.Backend.BackendUtils

genAsm :: [AAsm] -> (String, Int, Int, Int) -> [String]
genAsm aasms fnContext =
  map (aasmToString fnContext) aasms

cmpAsm :: ALoc -> AVal -> String
cmpAsm loc val =
  "cmpl " ++ (avalToString val) ++ ", " ++ (alocToString loc)

aasmToString :: (String, Int, Int, Int) -> AAsm -> String

{-aasmToString aasm | Trace.trace (show aasm) False = undefined-}

aasmToString _ AAsm {aAssign = [loc], aOp = LogicalNot, aArgs = [arg]} =
  "  movl " ++ (avalToString arg) ++ ", " ++ (alocToString loc) ++ "\n  not " ++ (alocToString loc) ++ "\n  and $1, " ++ (alocToString loc) ++ "\n"

aasmToString _ AAsm {aAssign = [loc], aOp = Lt, aArgs = [arg]} =
  "  " ++ (cmpAsm loc arg) ++ "\n  setl " ++ (alocByteToString loc) ++ "\n"

aasmToString _ AAsm {aAssign = [loc], aOp = Lte, aArgs = [arg]} =
  "  " ++ (cmpAsm loc arg) ++ "\n  setle " ++ (alocByteToString loc) ++ "\n"

aasmToString _ AAsm {aAssign = [loc], aOp = Gt, aArgs = [arg]} =
  "  " ++ (cmpAsm loc arg) ++ "\n  setg " ++ (alocByteToString loc) ++ "\n"

aasmToString _ AAsm {aAssign = [loc], aOp = Gte, aArgs = [arg]} =
  "  " ++ (cmpAsm loc arg) ++ "\n  setge " ++ (alocByteToString loc) ++ "\n"

aasmToString _ AAsm {aAssign = [loc], aOp = Equ, aArgs = [arg]} =
  "  " ++ (cmpAsm loc arg) ++ "\n  sete " ++ (alocByteToString loc) ++ "\n"

aasmToString _ AAsm {aAssign = [loc], aOp = Neq, aArgs = [arg]} =
  "  " ++ (cmpAsm loc arg) ++ "\n  setne " ++ (alocByteToString loc) ++ "\n"

aasmToString _ AAsm {aAssign = [loc], aOp = Neg, aArgs = [arg]} =
  (aasmToString ("", 0, 0, 0) (AAsm {aAssign = [loc], aOp = Nop, aArgs = [arg]}) ++ "  " ++ (opToString Neg) ++ " " ++ (alocToString loc) ++ "\n")

aasmToString _ AAsm {aAssign = [loc], aOp = Div, aArgs = [snd]} = divModToString loc snd Div
aasmToString _ AAsm {aAssign = [loc], aOp = Mod, aArgs = [snd]} = divModToString loc snd Mod

aasmToString _ AAsm {aAssign = [loc], aOp = LShift, aArgs = [snd]} =
  (checkLt32 snd) ++ (checkGte0 snd) ++
  "  movb " ++ (avalByteToString snd) ++ ", %cl\n  " ++ (opToString LShift) ++ " %cl, " ++ (alocToString loc) ++ "\n"

aasmToString _ AAsm {aAssign = [loc], aOp = RShift, aArgs = [snd]} =
  (checkLt32 snd) ++ (checkGte0 snd) ++
  "  movb " ++ (avalByteToString snd) ++ ", %cl\n  " ++ (opToString RShift) ++ " %cl, " ++ (alocToString loc) ++ "\n"

aasmToString _ AAsm {aAssign = [loc], aOp = BitwiseNot, aArgs = [arg]} =
  "  " ++ (opToString BitwiseNot) ++ " " ++ (alocToString loc) ++ "\n"

aasmToString _ AAsm {aAssign = [loc], aOp = Add, aArgs = [arg]} = 
  if (isZero arg) 
    then ""
    else "  " ++ (opToString Add) ++ " " ++ (avalToString arg) ++ ", "  ++ (alocToString loc) ++ "\n"
  where 
    isZero (AImm 0) = True
    isZero _ = False

aasmToString _ AAsm {aAssign = [loc], aOp = Nop, aArgs = [arg]} =
  if (argEq loc arg)
    then ""
    else "  " ++ (opToString Nop) ++ " " ++ (avalToString arg) ++ ", "  ++ (alocToString loc) ++ "\n"
  where
    argEq loc (ALoc loc') = loc == loc'
    argEq _ _ = False

aasmToString _ AAsm {aAssign = [loc], aOp = op, aArgs = [arg]} =
  "  " ++ (opToString op) ++ " " ++ (avalToString arg) ++ ", "  ++ (alocToString loc) ++ "\n"

aasmToString (fnName, _, _ ,_) (ACtrl (ALabel i)) =
  "\n" ++ fnName ++ "label" ++ show i ++ ":\n"

aasmToString (fnName, _, _, _) (ACtrl (AGoto i)) =
  "  jmp " ++ fnName ++ "label" ++ show i ++ "\n"

aasmToString (fnName, _, _, _) (ACtrl (AIf aval label)) =
  "  testb " ++ (avalByteToString aval) ++ ", " ++ (avalByteToString aval) ++ "\n  jnz " ++ fnName ++ "label" ++ (show label) ++ "\n"

aasmToString (_, size, numArgs, m) (ACtrl (ARet _)) =
  concat [addStr, genFnEpilogues numArgs m, "  ret\n"]
  where
    addStr = if (size > 0)
               then "  addq $" ++ show size ++ ", %rsp\n"
               else ""

aasmToString (_, size, numArgs, m) (AFnCall fnName loc locs lives) =
    prologue ++ "  call " ++ fnName ++ "\n  movl %eax, %r15d\n" ++ addSize ++ (genEpilogues loc m lives) ++ "  movl %r15d, " ++ (alocToString loc) ++ "\n"
  where 
    (prologue, size) = genProlugues loc locs m lives
    addSize = if (size > 0) 
                then "  addq $" ++ show (size * 8) ++ ", %rsp\n" 
                else ""

genArgPrologue' :: Int -> ALoc -> (String, Int, Int) -> (String, Int, Int)
genArgPrologue' shift loc (prolog, i, j) =
  let
    newPro = if i > 6 then "  movq " ++ (alocToQString loc) ++ ", %r15\n  movq %r15, -" ++ show ((j + shift + 1) * 8) ++ "(%rsp)\n"
                      else ""
    j' = if i > 6 then j + 1
                  else j
  in
    (prolog ++ newPro, i-1, j')

genFnEpilogues :: Int -> Int -> String
genFnEpilogues numArgs m =
  let
    callees' = take (max 0 (m - 6)) callees
    rest = concatMap genEpilogueIns (reverse callees')
    popBP = if numArgs > 6 then "  popq %rbp\n"
                           else ""
    n = if numArgs > 6 then (length callees') + 1
                       else length callees'
    buffer = if n `mod` 2 == 0 then incrStack8
                               else ""
  in
    buffer ++ rest ++ popBP

notSpilled :: ALoc -> Bool
notSpilled (AReg i) = i < spill_reg_num
notSpilled _ = False

genProlugues :: ALoc -> [ALoc] -> Int -> [ALoc] -> (String, Int)
genProlugues loc locs maxColor lives =
  let
    reg = alocToQString loc
    pushedArgs = max 0 ((length locs) - 6)
    callers' = filter (\r -> reg /= r) callers
    lives' = filter notSpilled lives
    liveRegs = map alocToQString lives'
    regsUsed = take (maxColor + 1) (map snd regQList)
    callers'' = filter (\r -> r `elem` liveRegs) callers'
    t = pushedArgs + (length callers'')
    shift = t `mod` 2
    (asms, i) = foldl (moveStack 0) ("", 0) callers''
    (asms', _, s) = foldr (genArgPrologue' shift) (asms, length locs, i) locs
    s' = s + shift
    asms'' = if (s' > 0)
               then asms' ++ "  subq $" ++ show ((s') * 8) ++ ", %rsp\n"
               else asms'
  in
    (asms'', s' - i)

genEpilogues :: ALoc -> Int -> [ALoc] -> String
genEpilogues loc maxColor lives =
  let
    reg = alocToQString loc
    callers' = filter (\r -> reg /= r) (reverse callers)
    regsUsed = take (maxColor + 1) (map snd regQList)
    lives' = filter notSpilled lives
    liveRegs = map alocToQString lives'
    callers'' = filter (\r -> r `elem` liveRegs) callers'
    epilogues = concatMap genEpilogueIns callers''
  in
    if (length callers'') `mod` 2 == 0 then epilogues
                                       else epilogues

avalByteToString :: AVal -> String
avalByteToString aval =
  case aval of ALoc loc -> (alocByteToString loc)
               _ -> (avalToString aval)

avalToString :: AVal -> String
avalToString aval =
  case aval of ALoc loc -> alocToString loc
               ABool b -> if b then "$1"
                               else "$0"
               AImm i -> "$" ++ (show i)

safeLookup :: Int -> Map.Map Int String -> String -> String
safeLookup i map s =
  case Map.lookup i map of Nothing -> error ("NOT FOUND " ++ (show i) ++ " wtf " ++ s)
                           Just r -> r
alocByteToString :: ALoc -> String
alocByteToString ASpill =
  safeLookup spill_reg_num regByteMap "SPILL"
alocByteToString (AReg i) =
  safeLookup i regByteMap "FUCK"
alocByteToString (AMem i) =
  alocToString (AMem i)

alocToQString :: ALoc -> String
alocToQString (AReg i) =
  safeLookup i regQMap "SHIT"
alocToQString (AMem i) =
  alocToString (AMem i)

alocToString :: ALoc -> String
alocToString (AArg i) =
  (show ((i + 2) * 8)) ++ "(%rbp)"
alocToString ASpill =
  safeLookup spill_reg_num regMap "SPILL"
alocToString (AReg i) =
  safeLookup i regMap "SHIT"
alocToString (AMem i) =  (show ((i - 1) * 8)) ++ "(%rsp)"
alocToString (ATemp i) =
  error "There's still an temp!"

divModToString :: ALoc -> AVal -> Op -> String
divModToString fst snd op = (divPrologue fst snd) ++ (divEpilogue fst op)

divPrologue :: ALoc -> AVal -> String
divPrologue fst snd =
  "  " ++ "movl" ++ " " ++ (alocToString fst) ++ ", " ++ "%eax" ++ "\n" ++
  "  " ++ "cltd" ++ "\n" ++
  "  " ++ (opToString Div) ++ " " ++ (avalToString snd) ++ "\n"

divEpilogue :: ALoc -> Op -> String
divEpilogue fst op
  | op == Div = "  " ++ "movl" ++ " " ++ "%eax" ++ "," ++ (alocToString fst) ++ "\n"
  | op == Mod = "  " ++ "movl" ++ " " ++ "%edx" ++ "," ++ (alocToString fst) ++ "\n"

opToString :: Op -> String
opToString op =
  case op of Mul -> "imull"
             Add -> "addl"
             Sub -> "subl"
             Div -> "idivl"
             Neg -> "negl"
             Mod -> "idivl"
             Nop -> "movl"
             RShift -> "sarl"
             LShift -> "sall"
             BitwiseAnd -> "andl"
             BitwiseOr -> "orl"
             BitwiseNot -> "notl"
             BitwiseXOr -> "xorl"
             Incr -> "addl"
             Decr -> "subl"
             _ -> error ("error matching " ++ show op)

checkGte0 :: AVal -> String
checkGte0 aval =
  "  cmpl $0, " ++ (avalToString aval) ++ "\n  jl error\n"

checkLt32 :: AVal -> String
checkLt32 aval =
  "  cmpl $32, " ++ (avalToString aval) ++ "\n  jge error\n"
