module Queue (Queue, newQueue, front, leave, enter) where

data Queue a = NullQ | Q a (Queue a)
	deriving (Show)

newQueue :: Queue a
newQueue = NullQ

front :: Queue a -> a
front NullQ = error "Queue empty."
front (Q x xs) = x

leave :: Queue a -> Queue a
leave NullQ = NullQ
leave (Q x xs) = xs

enter :: a -> Queue a -> Queue a
enter x NullQ = Q x NullQ
enter x (Q y ys) = Q y (enter x ys)

q = enter 8 (enter 3 (enter 5 newQueue))