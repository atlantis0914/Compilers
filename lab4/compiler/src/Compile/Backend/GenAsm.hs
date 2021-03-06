module Compile.Backend.GenAsm where

import Compile.Types
import qualified Data.Map as Map
import qualified Debug.Trace as Trace

import Compile.Backend.Registers
import Compile.Backend.BackendUtils

getLocSize :: ALoc -> Bool
getLocSize (AReg _ b) = b
getLocSize (ATemp _ b) = b
getLocSize (ASpill b) = b
getLocSize (AUtil b) = b
getLocSize (AIndex b) = b
getLocSize (AArg _ b) = b
getLocSize (APtr _ _ _ _ b) = b
getLocSize (AMem _ b) = b

genAsm :: [AAsm] -> (String, Int, Int, Int) -> [String]
genAsm aasms fnContext =
  map (aasmToString fnContext) aasms

cmpAsm :: ALoc -> AVal -> String
cmpAsm loc val =
  if getLocSize loc 
    then "cmpq " ++ (avalToString val) ++ ", " ++ (alocToString loc)
    else "cmpl " ++ (avalToString val) ++ ", " ++ (alocToString loc)

aasmToString :: (String, Int, Int, Int) -> AAsm -> String

aasmToString _ (AAsm [loc] LogicalNot [arg]) =
  "  " ++ (opToString Nop loc) ++ " " ++ (avalToString arg) ++ ", " ++ (alocToString loc) ++ "\n  not " ++ (alocToString loc) ++ "\n  and $1, " ++ (alocToString loc) ++ "\n"

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
  (aasmToString ("", 0, 0, 0) (AAsm {aAssign = [loc], aOp = Nop, aArgs = [arg]}) ++ "  " ++ (opToString Neg loc) ++ " " ++ (alocToString loc) ++ "\n")

aasmToString _ AAsm {aAssign = [loc], aOp = Div, aArgs = [snd]} = divModToString loc snd Div
aasmToString _ AAsm {aAssign = [loc], aOp = Mod, aArgs = [snd]} = divModToString loc snd Mod

aasmToString _ AAsm {aAssign = [loc], aOp = LShift, aArgs = [snd]} =
  (checkLt32 snd) ++ (checkGte0 snd) ++
  "  movb " ++ (avalByteToString snd) ++ ", %cl\n  " ++ (opToString LShift loc) ++ " %cl, " ++ (alocToString loc) ++ "\n"

aasmToString _ AAsm {aAssign = [loc], aOp = RShift, aArgs = [snd]} =
  (checkLt32 snd) ++ (checkGte0 snd) ++
  "  movb " ++ (avalByteToString snd) ++ ", %cl\n  " ++ (opToString RShift loc) ++ " %cl, " ++ (alocToString loc) ++ "\n"

aasmToString _ AAsm {aAssign = [loc], aOp = BitwiseNot, aArgs = [arg]} =
  "  " ++ (opToString BitwiseNot loc) ++ " " ++ (alocToString loc) ++ "\n"

aasmToString _ AAsm {aAssign = [loc], aOp = Add, aArgs = [arg]} = 
  if (isZero arg) 
    then ""
    else "  " ++ (opToString Add loc) ++ " " ++ (avalToString arg) ++ ", "  ++ (alocToString loc) ++ "\n"
  where 
    isZero (AImm 0) = True
    isZero _ = False

aasmToString _ AAsm {aAssign = [loc], aOp = Nop, aArgs = [arg]} =
  if (argEq loc arg)
    then ""
    else "  " ++ (opToString Nop loc) ++ " " ++ (avalToString arg) ++ ", "  ++ (alocToString loc) ++ "\n"
  where
    argEq loc (ALoc loc') = loc == loc'
    argEq _ _ = False

aasmToString _ AAsm {aAssign = [loc], aOp = op, aArgs = [arg]} =
  "  " ++ (opToString op loc) ++ " " ++ (avalToString arg) ++ ", "  ++ (alocToString loc) ++ "\n"

aasmToString (fnName, _, _ ,_) (ACtrl (ALabel i)) =
  "\n" ++ fnName ++ "label" ++ show i ++ ":\n"

aasmToString (fnName, _, _, _) (ACtrl (AGoto i)) =
  "  jmp " ++ fnName ++ "label" ++ show i ++ "\n"

aasmToString (fnName, _, _, _) (ACtrl (AIf aval label (Just l))) =
  "  testb " ++ (avalByteToString aval) ++ ", " ++ (avalByteToString aval) ++ "\n  jnz " ++ l ++ "\n"

aasmToString (fnName, _, _, _) (ACtrl (AIf aval label Nothing)) =
  "  testb " ++ (avalByteToString aval) ++ ", " ++ (avalByteToString aval) ++ "\n  jnz " ++ fnName ++ "label" ++ (show label) ++ "\n"

aasmToString (_, size, numArgs, m) (ACtrl (ARet _)) =
  concat [addStr, genFnEpilogues numArgs m, "  ret\n"]
  where
    addStr = if (size > 0)
               then "  addq $" ++ show size ++ ", %rsp\n"
               else ""

aasmToString (_, size, numArgs, m) (AFnCall fnName loc locs lives) =
    prologue ++ "  call " ++ fnName ++ "\n  " ++ (opToString Nop loc) ++ " " ++
    alocToString (AReg 0 (getLocSize loc)) ++ ", " ++ 
    (alocToString (ASpill (getLocSize loc))) ++ 
    "\n" ++ addSize ++ (genEpilogues loc m lives) ++ "  " ++ 
    (opToString Nop loc) ++ " " ++ (alocToString (ASpill (getLocSize loc))) ++
    ", " ++ (alocToString loc) ++ "\n"
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
notSpilled (AReg i b) = i < spill_reg_num
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
alocByteToString (ASpill b) =
  safeLookup spill_reg_num regByteMap "SPILL"
alocByteToString (AReg i b) =
  safeLookup i regByteMap "FUCK"
alocByteToString (AMem i b) =
  alocToString (AMem i b)
alocByteToString wut = ""

alocToQString :: ALoc -> String
alocToQString (AReg i b) = safeLookup i regQMap "SHIT"
alocToQString (AMem i b) = alocToString (AMem i b)
alocToQString (APtr base _ _ _ b) = alocToQString base
alocToQString (AIndex b) = safeLookup index_reg_num regQMap "SHIT"
alocToQString (AUtil b) = safeLookup util_reg_num regQMap "SHIT"
alocToQString (ASpill b) = safeLookup spill_reg_num regQMap "SHIT"

alocToString :: ALoc -> String
alocToString (AArg i b) = (show ((i + 2) * 8)) ++ "(%rbp)"
alocToString (ASpill b) = if b then alocToQString (ASpill b)
                               else safeLookup spill_reg_num regMap "SPILL"
alocToString (AIndex b) = if b then alocToQString (AIndex b)
                               else safeLookup index_reg_num regMap "INDEX"
alocToString (AUtil b) = if b then alocToQString (AUtil b)
                              else safeLookup util_reg_num regMap "INDEX"
alocToString (AReg i b) = if b then alocToQString (AReg i b)
                               else safeLookup i regMap "SHIT"
alocToString (AMem i b) =  (show ((i - 1) * 8)) ++ "(%rsp)"
alocToString (ATemp i b) = error "There's still an temp!"
alocToString (APtr base Nothing scale offset b) = show scale ++ "(" ++ alocToString base ++ ")"
alocToString (APtr base (Just index) scale offset b) = show offset ++ "(" ++ alocToString base ++ "," ++ alocToQString index ++ "," ++ show scale ++ ")"
alocToString loc = error (show loc ++ " EXHAUSTED")

divModToString :: ALoc -> AVal -> Op -> String
divModToString fst snd op = (divPrologue fst snd) ++ (divEpilogue fst op)

divPrologue :: ALoc -> AVal -> String
divPrologue fst snd =
  "  " ++ (opToString Nop fst) ++ " " ++ (alocToString fst) ++ ", " ++ alocToString (AReg 0 (getLocSize fst)) ++ "\n" ++
  "  " ++ "cltd" ++ "\n" ++
  "  " ++ (opToString Div fst) ++ " " ++ (avalToString snd) ++ "\n"

divEpilogue :: ALoc -> Op -> String
divEpilogue fst op
  | op == Div = "  " ++ (opToString Nop fst) ++ " " ++ alocToString (AReg 0 (getLocSize fst)) ++ "," ++ (alocToString fst) ++ "\n"
  | op == Mod = "  " ++ (opToString Nop fst) ++ " " ++ alocToString (AReg 2 (getLocSize fst)) ++ "," ++ (alocToString fst) ++ "\n"

opToString :: Op -> ALoc -> String
opToString op loc =
  if getLocSize loc 
    then case op of Mul -> "imulq"
                    Add -> "addq"
                    Sub -> "subq"
                    Div -> "idivq"
                    Neg -> "negq"
                    Mod -> "idivq"
                    Nop -> "movq"
                    RShift -> "sarq"
                    LShift -> "salq"
                    BitwiseAnd -> "andq"
                    BitwiseOr -> "orq"
                    BitwiseNot -> "notq"
                    BitwiseXOr -> "xorq"
                    Incr -> "addq"
                    Decr -> "subq"
                    _ -> error ("error matching " ++ show op)
    else case op of Mul -> "imull"
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
