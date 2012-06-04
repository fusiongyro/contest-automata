module Main where

tests = ["01", "100", "1010", "1111"]
main = mapM_ (putStrLn . start) tests

-- generated states below
q0, q1, q2, q3 :: String -> String
q0 ('1':xs) = q3 xs
q0 ('0':xs) = q1 xs
q0 [] = "Accept"

q1 ('1':xs) = q2 xs
q1 ('0':xs) = q3 xs
q1 [] = "Fail"

q2 ('1':xs) = q3 xs
q2 ('0':xs) = q1 xs
q2 [] = "Accept"

q3 ('1':xs) = q3 xs
q3 ('0':xs) = q3 xs
q3 [] = "Fail"

start = q0
