{- Copyright © 2015 Russell Dillin

surjections is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

surjections is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with surjections. If not, see <http://www.gnu.org/licenses/>. -}

c :: Int -> Int -> Int
c n i
    | i == 0 || n == 0 || i == n = 1
    | i  > 0 && i  < n           = c (n-1) (i-1) + (c (n-1) i)

e :: Int -> Int -> Int -> Int
e n m i = e' n m i 0

e' :: Int -> Int -> Int -> Int -> Int
e' n m i w
    | i > (n-1) = w
    | otherwise = e' n m (i+1) (w + (\x y z -> (c x z) * (s y z)) n m i)

s :: Int -> Int -> Int
s m n
    | n == 1          = 1
    | m  < n          = 0
    | m == n          = (\x -> foldl (*) 1 [1..x]) m
    | n  > 1 && m > n = (n^m) - (e n m 1)
    