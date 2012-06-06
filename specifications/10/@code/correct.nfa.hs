q0:: [Int] -> IO()
q1:: [Int] -> IO()
q2:: [Int] -> IO()
q3:: [Int] -> IO()

x = [1,0,0,0,1]
y = [0,0,1,0,0]
z = [1,1,0,0,0,1,1,1,0,0]
w = [1,1,0,0,0,1,0,0,0,1]

test = [x,y,z,w]

main = mapM q0 test

q3 [] = print "Solution"
q3 (0:xs) = q1 xs
q3 (1:xs) = q3 xs

q0 [] = print "Not Solution"
q0 (0:xs) = q1 xs
q0 (1:xs) = q0 xs

q1 [] = print "Not Solution"
q1 (0:xs) = q2 xs
q1 (1:xs) = q0 xs

q2 [] = print "Not Solution"
q2 (0:xs) = q3 xs
q2 (1:xs) = q0 xs

