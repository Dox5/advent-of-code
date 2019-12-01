module Tape(Tape(Tape),
            fromList,
            toList,
            setOffset,
            backward,
            forward,
            delete,
            current,
            insert) where

import Data.List (find, intersperse, splitAt)

import Data.Foldable ()

data Tape a = Tape {prefix :: [a], -- Reversed prefix (first element is previous)
                    current :: a,  -- The currently looked at item
                    suffix :: [a]  -- The next items
                   }

compose :: [(a -> a)] -> a -> a
compose = foldr (.) id

instance Show a => Show (Tape a) where
  showsPrec _ (Tape p c s)
    = let
        showList = compose . intersperse (showString ", ") . map shows
        pre = showList . reverse $ p
        suf = showList s

      in ("[" ++) . pre . (", |" ++) . shows c . ("|, " ++) . suf . ("]" ++)

instance Foldable Tape where
  foldr f z tape
    = let
      suff = foldr f z (suffix tape)
      cur  = f (current tape) suff 
      pre  = foldr f cur . reverse . prefix $ tape

    in pre

instance Eq a => Eq (Tape a) where
  (==) l r
    = let
        preEq = (prefix l)  == (prefix r)
        curEq = (current l) == (current r)
        sufEq = (suffix l)  == (suffix r)
      in preEq && curEq && sufEq
    

fromList :: [a] -> Tape a
fromList [] = error "Tape cannot be empty"
fromList (c:elms) = let
                    in Tape { prefix = [],
                              current = c,
                              suffix = elms}

toList :: Tape a -> [a]
toList (Tape p c s) = concat [p, c:s]

setOffset :: Int -> Tape a -> Tape a
setOffset i (Tape p c s)
  | i >= (length (Tape p c s)) = error "Index past end of tape"
  | i < 0 = error "Cannot take negative index of tape"
  | i < prefixLen
    = let
      p' = drop (prefixLen - i) p
      c' = p !! i
      s' = (take (prefixLen - i + 1) p) ++ c:s
    in Tape p' c' s'
  | i > prefixLen
    = let
        p' = (reverse . take (i - prefixLen -1) $ s) ++ c:p
        c' = s !! (i - prefixLen - 1)
        s' = drop (i - prefixLen) s
      in Tape p' c' s'

  | i == prefixLen = Tape p c s
  | otherwise = error "Poorly implemented :("
    where
      prefixLen = length p

next :: Tape a -> Tape a
next t@(Tape [] _ []) = t
next (Tape p c []) = let (c':s') = reverse (c:p) in Tape [] c' s'
next (Tape p c s) = let (c':s') = s in Tape (c:p) c' s'

forward :: Int -> Tape a -> Tape a
forward 0 t = t
forward n t = forward (n - 1) $ next t

previous :: Tape a -> Tape a
previous t@(Tape [] _ []) = t
previous (Tape [] c s) = let (c':p') = reverse (c:s) in Tape p' c' []
previous (Tape p c s) = let (c':p') = p in Tape p' c' (c:s)

backward :: Show a => Int -> Tape a -> Tape a
backward 0 t = t
backward n t = backward (n - 1) $ previous t


-- Delete the 'current' element, move remaining elements left
delete :: Tape a -> Tape a
delete (Tape p c s)
  | null p && null s = error "Cannot delete last element of tape"
  | null s           = let
                         s = reverse p
                       in Tape [] (head s) (drop 1 s)
  | otherwise        = Tape p (head s) (drop 1 s)

insert :: a -> Tape a -> Tape a
insert elem (Tape p c s) = Tape p c (elem:s)

