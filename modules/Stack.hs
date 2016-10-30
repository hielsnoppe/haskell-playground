module Stack (Stack, newStack, top, pop, push) where

data Stack a = NullS | S a (Stack a)
	deriving (Show)

newStack :: Stack a
newStack = NullS

top :: Stack a -> a
top NullS = error "Stack empty."
top (S x xs) = x

pop :: Stack a -> Stack a
pop NullS = NullS
pop (S x xs) = xs

push :: a -> Stack a -> Stack a
push x NullS = (S x NullS)
push x xs = (S x xs)

s = push 8 (push 3 (push 5 newStack))