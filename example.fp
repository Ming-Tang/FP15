fib  = (<= 1) : 1 | [(-1), (-2)] @fib +
fib1 = [_, 1] <= : 1 | [[_, 1] -, [_, 2] -] @fib +
fib2 = [_, 1] <= : 1 | [[_, 1] - fib, [_, 2] - fib] +
fib3 = (If (Compose (Fork _ 1) <=)
           1
           (Compose (Fork (Compose (Fork _ 1) -)
                          (Compose (Fork _ 2) -))
                    (Map fib)
                    +))

fac = (< 1) : 1 | (* {(-1) fac})
fac1 = [_, 1] < : 1 | [_, [_, 1] - fac] *
fac2 = (If (Compose (Fork _ 1) <)
           1
           (Compose (Fork _ (Compose pred fac)) *))

avg = (sum % len)
-- TODO duplicate definitions are not reported
avg1 = [sum, len] %

dblPlusReci = (2* + 1%)
dblPlusReci1 = (*2 + 1%)
dblPlusReci2 = (1% + *2)
dblPlusReci3 = ((*2) + (1%))
dblPlusReci4 = ((_*2) + (1%_))
dblPlusReci5 = [[_,2]*, [1,_]%] +

main = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]
       [[avg, avg1], @[fib, fib1, fib2, fib3],
                     @[fac, fac1, fac2],
                     @[dblPlusReci, dblPlusReci1, dblPlusReci2, dblPlusReci3,
                       dblPlusReci4, dblPlusReci5]]

