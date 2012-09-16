-- one application of Jeff's procedure will, on average, reduce
-- the amount of pie by 2/3.

funcE :: Float -> Float
funcE x = let f = 1/x
          in (log f) / (log (2/3))
