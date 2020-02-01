-- | The test generator boilerplate module.
--
-- Any test that is supported (HUnit, HSpec, etc.) provides here, a
-- generator type with all the context necessary for outputting the
-- necessary boilerplate for the generated main function that will
-- run all the tests.
module Test.Tasty.Generator
  ( -- * Types
    Generator (..),
    Test (..),

    -- * Generators
    generators,
    getGenerator,
    getGenerators,

    -- * Boilerplate Formatter
    showSetup,

    -- * Type Constructor
    mkTest,
  )
where

import Data.Function
import Data.List
import Data.Maybe
import System.FilePath

-- | The test type.
data Test
  = Test
      { -- | Module name.
        testModule :: String,
        -- | Function name.
        testFunction :: String
      }
  deriving (Eq, Show, Ord)

-- | 'Test' constructor.
mkTest :: FilePath -> String -> Test
mkTest = Test . replacePathSepTo '.' . dropExtension
  where
    replacePathSepTo c1 = map $ \c2 -> if isPathSeparator c2 then c1 else c2

-- | The generator type.
data Generator
  = Generator
      { -- | Generator prefix.
        generatorPrefix :: String,
        -- | Module import path.
        generatorImport :: String,
        -- | Generator class.
        generatorClass :: String,
        -- | Generator setup.
        generatorSetup :: Test -> String
      }

-- | Module import qualifier.
qualifyFunction :: Test -> String
qualifyFunction t = testModule t ++ "." ++ testFunction t

-- | Function namer.
name :: Test -> String
name = chooser '_' ' ' . tail . dropWhile (/= '_') . testFunction
  where
    chooser c1 c2 = map $ \c3 -> if c3 == c1 then c2 else c3

-- | Generator retriever (single).
getGenerator :: Test -> Generator
getGenerator t = fromJust $ getPrefix generators
  where
    getPrefix = find ((`isPrefixOf` testFunction t) . generatorPrefix)

-- | Generator retriever (many).
getGenerators :: [Test] -> [Generator]
getGenerators =
  map head
    . groupBy ((==) `on` generatorPrefix)
    . sortOn generatorPrefix
    . map getGenerator

-- | Boilerplate formatter.
showSetup :: Test -> String -> String
showSetup t var = "  " ++ var ++ " <- " ++ setup ++ "\n"
  where
    setup = generatorSetup (getGenerator t) t

-- | All types of tests supported for boilerplate generation.
generators :: [Generator]
generators =
  [ quickCheckPropertyGenerator,
    smallCheckPropertyGenerator,
    hedgehogPropertyGenerator,
    hunitTestCaseGenerator,
    hspecTestCaseGenerator,
    tastyTestGroupGenerator
  ]

-- | Quickcheck group generator prefix.
hedgehogPropertyGenerator :: Generator
hedgehogPropertyGenerator =
  Generator
    { generatorPrefix = "hprop_",
      generatorImport = "import qualified Test.Tasty.Hedgehog as H\n",
      generatorClass = "",
      generatorSetup = \t -> "pure $ H.testProperty \"" ++ name t ++ "\" " ++ qualifyFunction t
    }

-- | Quickcheck group generator prefix.
quickCheckPropertyGenerator :: Generator
quickCheckPropertyGenerator =
  Generator
    { generatorPrefix = "prop_",
      generatorImport = "import qualified Test.Tasty.QuickCheck as QC\n",
      generatorClass = "",
      generatorSetup = \t -> "pure $ QC.testProperty \"" ++ name t ++ "\" " ++ qualifyFunction t
    }

-- | Smallcheck group generator prefix.
smallCheckPropertyGenerator :: Generator
smallCheckPropertyGenerator =
  Generator
    { generatorPrefix = "scprop_",
      generatorImport = "import qualified Test.Tasty.SmallCheck as SC\n",
      generatorClass = "",
      generatorSetup = \t -> "pure $ SC.testProperty \"" ++ name t ++ "\" " ++ qualifyFunction t
    }

-- | HUnit generator prefix.
hunitTestCaseGenerator :: Generator
hunitTestCaseGenerator =
  Generator
    { generatorPrefix = "unit_",
      generatorImport = "import qualified Test.Tasty.HUnit as HU\n",
      generatorClass =
        concat
          [ "class TestCase a where testCase :: String -> a -> IO T.TestTree\n",
            "instance TestCase (IO ())                      where testCase n = pure . HU.testCase      n\n",
            "instance TestCase (IO String)                  where testCase n = pure . HU.testCaseInfo  n\n",
            "instance TestCase ((String -> IO ()) -> IO ()) where testCase n = pure . HU.testCaseSteps n\n"
          ],
      generatorSetup = \t -> "testCase \"" ++ name t ++ "\" " ++ qualifyFunction t
    }

-- | Hspec generator prefix.
hspecTestCaseGenerator :: Generator
hspecTestCaseGenerator =
  Generator
    { generatorPrefix = "spec_",
      generatorImport = "import qualified Test.Tasty.Hspec as HS\n",
      generatorClass = "",
      generatorSetup = \t -> "HS.testSpec \"" ++ name t ++ "\" " ++ qualifyFunction t
    }

-- | Tasty group generator prefix.
tastyTestGroupGenerator :: Generator
tastyTestGroupGenerator =
  Generator
    { generatorPrefix = "test_",
      generatorImport = "",
      generatorClass =
        concat
          [ "class TestGroup a where testGroup :: String -> a -> IO T.TestTree\n",
            "instance TestGroup T.TestTree        where testGroup _ a = pure a\n",
            "instance TestGroup [T.TestTree]      where testGroup n a = pure $ T.testGroup n a\n",
            "instance TestGroup (IO T.TestTree)   where testGroup _ a = a\n",
            "instance TestGroup (IO [T.TestTree]) where testGroup n a = T.testGroup n <$> a\n"
          ],
      generatorSetup = \t -> "testGroup \"" ++ name t ++ "\" " ++ qualifyFunction t
    }
