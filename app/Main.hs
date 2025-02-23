module Main where

import Lib

data Tree a  = Empty | Node a (Tree a) (Tree a) deriving (Show)



freeTree :: Tree Char

freeTree = 
    Node 'P'
        (Node 'O'
            (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )
changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n ) r)) = (Node x l (Node y (Node 'P' m n ) r)) 

data Direction =L |R deriving (Show)

type Directions = [Direction]
changeToPd :: Directions -> Tree Char -> Tree Char
changeToPd ( L:ds) (Node x l r) = Node x (changeToPd ds l) r

changeToPd ( R:ds) (Node x l r) = Node x l (changeToPd ds r)

changeToPd [] (Node _ l r) = Node 'P' l r

elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R: ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

newTree = changeToPd [L,R] freeTree

type BreadCrumbs = [Direction]

goLeft :: (Tree a, BreadCrumbs) -> (Tree a, BreadCrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, BreadCrumbs) -> (Tree a, BreadCrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)

main :: IO  ()
main = someFunc
