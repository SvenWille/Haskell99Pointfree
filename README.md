# Haskell99Pointfree

####!!!Work in progress!!!
##Useful functions:

- **ifM (from Control.Monad.Extra):**
- **liftA(n)/liftM(n):**
- **if'/ifThenElse (from Data.Bool.HT (package utilities-ht)):**
- **over (from Control.Lens):**
- **set (from Control.Lens):**
- **view (from Control.Lens):**

I will present only some of my solutions. For more solutions have a look at the sources.

In some cases I decided to split up the one-line to make it more readable.

Problem 1: Find the last element of a list 

```Haskell
--using ifM from Control.Monad.Extra
p01_6 :: [a] -> Maybe a
p01_6 = ifM null (const Nothing) (Just . last)

p01_8 :: [a] -> Maybe a
p01_8 = flip if' Nothing . null <*>  Just .last
```

Problem 2: Find the last but one element of a list

```Haskell
--using (<*>) from Control.Applicative
p02_3A :: [a] -> Maybe a
p02_3A =   flip if' Nothing . (< 2) . length <*>  Just . last . init

--using liftA3 from Control.Applicatice
p02_4 :: [a] -> Maybe a
p02_4 = liftA3 ifThenElse ( (<=) 2  . length ) (Just . head . tail . reverse ) (const Nothing)
```

Problem 3: Return the nth element of a list

```Haskell
p03_4 :: [a] -> Int -> Maybe a
p03_4 =   ( . subtract 1 ) .  ap ( liftM2 (flip ( `if'` Nothing))   (Just . head) .  ( . null) . (||) . (>) 0) . flip drop

p03_6 :: [a] -> Int -> Maybe a
p03_6 =  (fmap snd . ) . ( . ((. fst) . (==))) . flip find . zip [1..]
```

Problem 4: Return the length of a list

```Haskell
p04_7 :: [a] -> Int
p04_7 = sum . map (const 1)

--using fix from Control.Monad.Fix
p04_8 :: [a] -> Int
p04_8 = fix (ifM null (const 0) . ( . tail) . ((+1). ))
```

Problem 5: Reverse a list

```Haskell 
p05_2 :: [a] -> [a]
p05_2 = liftA3 ifThenElse null (const []) (  flip (flip (++) . take 1) =<< p05_2 . tail )
```

Problem 6: Check if a given list is a palindrom

```Haskell
--does redundant comparisons
p06_3 :: Eq a => [a] -> Bool
p06_3 = and . liftA2 (zipWith (==)) id reverse

p06_7 :: Eq a => [a] -> Bool
p06_7  = uncurry isPrefixOf . second reverse .  ap (flip splitAt) (flip div 2 . length)   
```

Problem 7: 

```Haskell 
--using Prisms
data NestedList a = List [NestedList a] | Elem a

makePrisms ''NestedList

--recursive
p07_1 :: NestedList a -> [a]
p07_1 =  ifM (is _Elem) ( (:[]) . fromJust . preview _Elem) (concatMap p07_1 . fromJust . preview _List)
```

Problem 8: 

```Haskell
p08_1 :: Eq a => [a] -> [a]
p08_1 = map head . group

p08_4 :: Eq a => [a] -> [a]
p08_4 = reverse . snd . until ( null . fst ) (liftA2 (,) ( join (dropWhile . (==) . head) . fst) (liftA2 (:) (head . fst)  snd ) )  . (,[])
```


Problem 9: 

```Haskell
--trivial solution
p09_1 :: Eq a => [a] -> [[a]]
p09_1 = group

p09_3 :: Eq a => [a] -> [[a]]
p09_3 = reverse . snd . until ( null . fst ) nextStep . (,[])
  where
    nextStep :: Eq a => ([a],[[a]]) -> ([a],[[a]])
    nextStep = liftA2 (,) (join (dropWhile . (==) . head) . fst) ( (flip (:) . snd) <*>   join (takeWhile    . (==) . head) . fst )
```

Problem 10:

```Haskell
p10_1 :: Eq a =>  [a] -> [(Integer,a)]
p10_1 = map (liftA2 (,) genericLength head) .  p09_1

--using arrows
p10_3 :: Eq a => [a] -> [(Int,a)]
p10_3 = map ( length &&& head ) . p09_3
```

Problem 11:
```Haskell
data ListItem a = Multiple Integer a  | Single a deriving Show

p11_1 :: Eq a =>  [a] -> [ListItem a]
p11_1 =  map ( ifM ( (==1) . fst) ( Single . snd) (liftA2 Multiple fst snd) ) .  p10_1
```