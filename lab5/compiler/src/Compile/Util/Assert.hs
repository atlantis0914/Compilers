module Compile.Util.Assert where 

assert :: Bool -> String -> a -> a
assert False msg x = error msg
assert _ _ x = x
