{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ArbitraryTokens where
import Control.Applicative hiding (Const)
import Test.QuickCheck

infixl 3 <++>
infixl 3 ++>
infixl 3 <++
infixr 4 <:>
infixr 4 -:>
infixr 4 <:-
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(++>) :: Applicative f => [a] -> f [a] -> f [a]
(<++) :: Applicative f => f [a] -> [a] -> f [a]
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(-:>) :: Applicative f => a -> f [a] -> f [a]
(<:-) :: Applicative f => f a -> [a] -> f [a]
a <++> b = (++) <$> a <*> b
a ++> b = pure a <++> b
a <++ b = a <++> pure b
a <:> b = (:) <$> a <*> b
a -:> b = pure a <:> b
a <:- b = a <:> pure b

star :: Gen [[a]] -> Gen [a]
star = fmap concat

one :: Gen a -> Gen [a]
one = fmap (:[])

opt :: Gen [a] -> Gen [a]
opt x = oneof [x, pure []]

opChars, identChars, digits :: String
opChars = "@!?~%^&*-+=<>/\\"
digits = ['0'..'9']
identChars = "_" ++ ['A'..'Z'] ++ ['a'..'z'] ++ digits

underscores :: Gen String
underscores = oneof [ pure "", pure "", pure "", pure "", listOf (pure '_') ]

arbDigit, arbOpChar :: Gen Char
arbIdent, arbFName, arbFlName, arbModName, arbOpName :: Gen String
arbDec, arbOct, arbHex, arbInteger, arbReal, arbModPart :: Gen String
arbWS, arbStr, arbOp, arbDotOp, arbF, arbFl :: Gen String
arbNum, arbHash :: Gen String

arbDigit = elements digits
arbOpChar = elements opChars

arbIdent = listOf1 (elements identChars)
arbFlName = underscores <++> elements ['A'..'Z'] <:> arbIdent
arbFName = underscores <++> elements ['a'..'z'] <:> arbIdent
arbModName = arbFlName

arbOpName = oneof [ listOf1 (elements opChars)
                  , '.' -:> listOf1 (elements ".")]

arbDec = oneof [ pure "0", arbDigit <:> listOf arbDigit ]
arbOct = elements ["#o", "#O"] <++> listOf1 (elements ['0'..'7'])
arbHex = elements ["#x", "#X"] <++> listOf1 arbHexDigit where
  arbHexDigit = elements "0123456789abcdefABCDEF"
arbInteger = elements ["~", ""] <++> oneof [arbDec, arbOct, arbHex]
arbReal
  = opt (pure "~") <++> arbDec <++> oneof [ arbFrac <++> arbExp
                                              , arbFrac, arbExp ] where
  arbFrac = '.' -:> listOf1 arbDigit
  arbExp = elements "eE" <:> opt (elements ["+", "-"]) <++> listOf1 arbDigit

arbModPart = arbModName <++> star (listOf (arbModName <++ ".")) <++ "."

arbWS = listOf $ elements " \t\v"
arbStr = "\"" ++> star (listOf strChar) <++ "\"" where
  strChar = do c <- arbitrary
               return $ if c == '\\' || c == '"'
                        then ['\\', c] else "c"

arbF = opt arbModPart <++> arbFName
arbFl = opt arbModPart <++> arbFlName
arbOp = oneof [ arbModPart <++ "(" <++> arbOpName <++ ")", arbOpName ]
arbDotOp = "." ++> opt arbModPart <++> arbFName

arbNum = oneof [ arbInteger, arbReal ]
arbHash = '#' -:> oneof [one arbOpChar, star (listOf1 arbIdent)]
