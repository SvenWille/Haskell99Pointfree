module Haskell99Pointfree.P47
    (
    ) where


import Haskell99Pointfree.P46

and_1' = and_1
or_1' = or_1
equ_1' = equ_1
nand_1' = nand_1
xor_1' = xor_1
impl_1' = impl_1


--I just declared the fixity to show how it is done, the values are not correct.

infixl 4 `and_1'`
infixl 6 `or_1'`
infixl 4 `equ_1'`
infixl 3 `nand_1'`
infix 7 `xor_1'`
infixr 2 `impl_1'`
