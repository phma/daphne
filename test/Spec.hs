{-# LANGUAGE InstanceSigs #-}
import Data.Word
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Cryptography.Daphne

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "invStep" $
      \x l r -> invStep (step x l r) l r == x
  ]

unitTests = testGroup "Unit tests"
  [ testCase "step49" $
      (step 49 243 125) @?= 75
  ]
