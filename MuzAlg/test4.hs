-- Suma liczb listy ns
mysum [] = 0
mysum (n : ns ) = n + sum ns
