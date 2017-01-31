--examples of list comprehension
--1) flattening a bidimensional list
flattening :: [[a]] -> [a]
flattening xss = [x | xs <- xss, x <- xs]