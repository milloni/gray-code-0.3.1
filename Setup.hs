#!/usr/bin/env runhaskell
import Distribution.Simple

import Test.QuickCheck
import Codec.Binary.Gray_props

main = defaultMainWithHooks $
       simpleUserHooks { runTests = tests }

tests _ _ _ _ = 
    quickCheckWith (stdArgs {maxSuccess = 1000}) all_props
