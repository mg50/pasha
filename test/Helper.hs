{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}

module Helper where
import qualified Data.Set as S
import Test.Hspec

class Equiv a b where
  equiv :: a -> b

instance (Equiv a c, Equiv b d) => Equiv (a, b) (c, d) where
  equiv (a, b) = (equiv a, equiv b)

instance (Ord a) => Equiv [a] (S.Set a) where
  equiv = S.fromList

shouldBeEquiv :: (Eq a, Show a, Equiv b a) => a -> b -> Expectation
shouldBeEquiv x y = x `shouldBe` equiv y
