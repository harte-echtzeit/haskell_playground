squareMe x = x * x

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = (if x > 100 then x else doubleMe x)

recurPrint x = do
  print x
  if not (null x) then recurPrint (tail x) else pure()
  -- a fun to recursively empty a list and print it

reOrgList n x = drop n x ++ take n x
maskPat l x = take l x
maskPat' o l x = take l (drop o x)

-- empties out a long pattern and then stops
maskIterNew l x = if length x > l then do
  print (maskPat' 0 l x)
  maskIterNew l (drop l x)
  else return () 

-- runs forever (remove print in final version)
maskIter' n x = do
  print(maskPat n x)
  maskIter' n (reOrgList n x)
 
--maskIter 0 l x = print (take l x)
--maskIter n l x = do
--  print(take l x)
--  maskIter (n-1) l (reOrgList l x)
-- here an if clause has to be inserted to catch the case that l>len(x)


-- maybe try to refactor here and return a tuple from fun?
--maskPat' 0 l x = (take l x, x)
--maskPat' n l (x, y) = (take l x, (maskPat' (n-1) l (reOrgList l x), 0))


-- simple CELLULAR AUTOMATA

-- pattern matching for one CA rule -> can it be automated?
-- is bound to strings amo-> make it general to lists?

--random rule made up by myself
--caPat "000" = '0'
--caPat "001" = '0'
--caPat "010" = '1'
--caPat "100" = '1'
--caPat "011" = '0'
--caPat "110" = '1'
--caPat "101" = '0'
--caPat "111" = '1'

-- rule 90
--caPat "000" = '0'
--caPat "001" = '1'
--caPat "010" = '0'
--caPat "100" = '1'
--caPat "011" = '1'
--caPat "110" = '1'
--caPat "101" = '0'
--caPat "111" = '0'

-- rule 110
caPat "000" = '0'
caPat "001" = '1'
caPat "010" = '1'
caPat "100" = '0'
caPat "011" = '1'
caPat "110" = '1'
caPat "101" = '1'
caPat "111" = '0'



-- very simple CA routine > take 3 elements of a pattern, look up rule and append result to old, reduced pattern
baseCA (x,y) = (take 3 x, tail x ++ [caPat (take 3 x)])
iterCA 0 x = return()
iterCA n x = do
  --print(fst (baseCA (x,0)))
  print(snd (baseCA (x,0)))
  iterCA (n-1) (snd (baseCA (x,0)))

-- simplified
baseCA' x = tail x ++ [caPat (take 3 x)]
iterCA' 0 x = return()
iterCA' n x = do
  print(baseCA' x)
  iterCA' (n-1) (baseCA' x)

-- 2D CA

expandList x = last x:x ++ [head x]

twoDca x y = if length x < 3 then reverse y else twoDca (tail x) (caPat (take 3 x):y)

-- only for display purposes
iterTwoD 0 x = return()
iterTwoD n x = do
  print (twoDca (expandList x) [])
  iterTwoD (n-1) (twoDca (expandList x) [])
