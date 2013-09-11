module Compile.Util.AbstractAssembly where

import Compile.Types

aLocFromAVal :: AVal -> Maybe ALoc
aLocFromAVal (ALoc loc) = Just loc
aLocFromAVal _ = Nothing
