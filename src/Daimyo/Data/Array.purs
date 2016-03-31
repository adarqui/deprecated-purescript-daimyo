module Daimyo.Data.Array (
  intersperse,
  prependToAll,
  break,
  unlines,
  lines,
  elem,
  elemBy
) where



import Prelude
import Data.Array
import Data.Maybe
import Data.String (joinWith, toCharArray, fromCharArray)
import Data.Tuple

-- | intersperse
--
-- >>> intersperse ',' "abcde" == "a,b,c,d,e"
--
intersperse :: forall a. a -> Array a -> Array a
intersperse sep ys  = case uncons ys of
                           Nothing                   -> []
                           Just { head: h, tail: t } -> h : prependToAll sep t

-- | prependToAll
--
prependToAll :: forall a. a -> Array a -> Array a
prependToAll sep ys = case uncons ys of
                           Nothing                   -> []
                           Just { head: h, tail: t } -> sep : h : prependToAll sep t

-- | 'unlines' is an inverse operation to 'lines'.
-- It joins lines, after appending a terminating neline to each.
unlines :: Array String -> String
unlines = joinWith "\n"

-- | 'lines' breaks a string up into a list of strings at newline
-- characters.  The resulting strings do not contain newlines.
lines :: String -> Array String
lines = reverse <<< map (fromCharArray <<< reverse) <<< go [] [] <<< toCharArray
  where
  go acc accs xs = case uncons xs of
                        Nothing -> if accs == [] then acc else (accs : acc)
                        Just {head:h, tail:t}
                          | h == '\n' -> let f = if accs == [] then acc else (accs : acc) in go f [] t
                          | otherwise -> go acc (h : accs) t

-- | break
--
-- break (\c -> c == '\n') (toCharArray "hello\nworld!")
--
break :: forall a. (a -> Boolean) -> Array a -> Tuple (Array a) (Array a)
break p [] = Tuple [] []
break p xs = case uncons xs of
                   Nothing -> Tuple [] []
                   Just {head:x, tail:xs'}
                    | p x       -> Tuple [] xs
                    | otherwise -> let r = break p xs' in case r of
                                       Tuple ys zs -> Tuple (x:ys) zs


-- | elem
--
elem :: forall a. Eq a => a -> Array a -> Maybe a
elem a = elemBy ((==) a)



-- | elemBy
--
elemBy :: forall a. Eq a => (a -> Boolean) -> Array a -> Maybe a
elemBy by l = go l
  where
    go [] = Nothing
    go z = case uncons z of
      Nothing -> Nothing
      Just {head:x, tail:xs} ->
        if by x
          then (Just x)
          else (go xs)
