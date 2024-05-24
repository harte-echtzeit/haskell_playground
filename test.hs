squareMe x = x * x

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = (if x > 100 then x else doubleMe x)

recurPrint x = do
  print x
  if not (null x) then recurPrint (tail x) else pure()
  -- a fun to recursively empty a list and print it

reOrgList n x = drop n x ++ take n x

maskPat 0 l x = print (take l x)
maskPat n l x = do
  print(take l x)
  maskPat (n-1) l (reOrgList l x)
-- here an if clause has to be inserted to catch the case that l>len(x)

-- maybe try to refactor here and return a tuple from fun?
--maskPat' 0 l x = (take l x, x)
--maskPat' n l (x, y) = (take l x, (maskPat' (n-1) l (reOrgList l x), 0))


-- simple cellular automata

-- later: fix pattern length due to spaces here ->5 means 3 chars 
nextCA x = take 3 x

-- pattern matching for one CA rule -> can it be automated?

caPat "000" = "1"
caPat "001" = "0"
caPat "010" = "1"
caPat "100" = "0"
caPat "011" = "1"
caPat "110" = "0"
caPat "101" = "1"
caPat "111" = "0"

caAround x = (caPat (nextCA x))

--iterCA 0 x = return[x]
--iterCA n x = do
  --caPat x ++ iterCA (n-1) x


something (x,y) = (take 3 x, drop 3 x ++ take 3 x)

someIter 0 (x,y) = return(([], []))
someIter n (x,y) = do
  something(x, y)
  someIter (n-1) (snd (something (x,y)), y)
