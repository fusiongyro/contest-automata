q0:: [Int] -> IO()
q1:: [Int] -> IO()
q2:: [Int] -> IO()
q3:: [Int] -> IO()

x = [0,0]
y = [0,1]
z = [1,0,1,0]
w = [0,1,0]

test = [x,y,z,w]

main = mapM q0 test

q0 [] = print "Solution"
q0 (0:xs) = q1 xs
q0 (1:xs) = q3 xs

q1 [] = print "Not Solution"
q1 (0:xs) = q0 xs
q1 (1:xs) = q2 xs

q2 [] = print "Not Solution"
q2 (0:xs) = q3 xs
q2 (1:xs) = q1 xs

q3 [] = print "Not Solution"
q3 (0:xs) = q2 xs
q3 (1:xs) = q0 xs

