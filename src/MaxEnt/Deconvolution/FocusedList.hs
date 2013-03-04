{-# LANGUAGE RecordWildCards #-}
module MaxEnt.Deconvolution.FocusedList where
import Data.Foldable
import Data.Maybe
import Control.Applicative
import Control.Monad

data FocusedList a = FocusedList {
        _reversedPrefix :: [a],
        _focus          :: a,
        _suffix         :: [a]
    }
    deriving(Show, Eq)

-- radius must be greater then one
neighborhood :: Int -> FocusedList a -> FocusedList a
neighborhood radius (FocusedList {..}) = 
    FocusedList (take (radius - 1) _reversedPrefix) 
                _focus 
                (take (radius - 1) _suffix)

previous :: FocusedList a -> Maybe (FocusedList a)
previous (FocusedList {..}) = case null _suffix of
    True  -> Nothing
    False -> Just $ FocusedList (_focus : _reversedPrefix) 
                                (head _suffix) 
                                (tail _suffix)

next :: FocusedList a -> Maybe (FocusedList a)
next (FocusedList {..}) = case null _reversedPrefix of
    True  -> Nothing
    False -> Just $ FocusedList (tail _reversedPrefix) 
                                (head _reversedPrefix) 
                                (_focus : _suffix)

stepBack :: Int -> FocusedList a -> Maybe (FocusedList a)
stepBack count xs = foldM (\x _ -> previous x) xs [0..count - 1] 

extendWith :: [a] -> a -> a -> FocusedList a
extendWith xs start end = 
    FocusedList (repeat start) (head xs) (tail xs ++ repeat end)

instance Foldable FocusedList where
    foldMap f (FocusedList {..}) = 
        foldMap f $ reverse _reversedPrefix ++ [_focus] ++ _suffix
        
shifts :: FocusedList a -> [FocusedList a]
shifts x = catMaybes . go $ Just x where
    go x = x : go (join $ next <$> x) 
















