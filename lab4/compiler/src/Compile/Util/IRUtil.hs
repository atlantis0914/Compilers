module Compile.Util.IRUtil where 


getName :: String -> String
getName (('_'):xs) = getName xs
getName (('c'):('0'):('_'):xs) = getName xs
getName s = s

