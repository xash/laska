module Helper where

isEven x = x `mod` 2 == 0
addTuple (x,y) (a,b) = (x+a,y+b)
subTuple (x,y) (a,b) = (x-a,y-b)
elem x a = any (\y -> x == y) a
find : (a -> Bool) -> [a] -> Maybe a
find f a = let filtered = filter f a in if isEmpty filtered then Nothing else Just (head filtered)
newPlace (x,y) (a,b) = (x+(x-a),y+(y-b))
