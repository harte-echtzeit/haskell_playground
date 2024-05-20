squareMe x = x * x

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = (if x > 100 then x else doubleMe x)

recurPrint x = do
  print x
  if not (null x) then recurPrint (tail x) else pure()
  -- a fun to recursively empty a list and print it

reOrgList n x = drop n x ++ take n x

maskPat n _ _ = pure()
        n l x = do
  take n (reOrgList n x)
  maskPat (n-1) l x

