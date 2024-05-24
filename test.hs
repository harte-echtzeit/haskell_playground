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


-- simple CELLULAR AUTOMATA

-- pattern matching for one CA rule -> can it be automated?
caPat "000" = "0"
caPat "001" = "0"
caPat "010" = "1"
caPat "100" = "1"
caPat "011" = "0"
caPat "110" = "1"
caPat "101" = "0"
caPat "111" = "1"


-- very simple CA routine > take 3 elements of a pattern, look up rule and append result to old, reduced pattern
baseCA (x,y) = (take 3 x, tail x ++ caPat (take 3 x))
iterCA 0 x = return()
iterCA n x = do
  --print(fst (baseCA (x,0)))
  print(snd (baseCA (x,0)))
  iterCA (n-1) (snd (baseCA (x,0)))

-- simplified
baseCA' x = tail x ++ caPat (take 3 x)
iterCA' 0 x = return()
iterCA' n x = do
  print(baseCA' x)
  iterCA' (n-1) (baseCA' x)
