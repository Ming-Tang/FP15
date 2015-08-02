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
fac1 = [_, 1] < : 1 | [_, pred fac] *
fac2 = (If (Compose (Fork _ 1) <)
           1
           (Compose (Fork _ (Compose pred fac)) <))

avg = (sum % len)
-- TODO duplicate definitions are not reported
avg1 = [sum, len] %

dblPlusReci = (2* + 1%)
dblPlusReci1 = (*2 + 1%)
dblPlusReci2 = (1% + *2)
dblPlusReci3 = ((*2) + (1%))
dblPlusReci4 = ((_*2) + (1%_))
dblPlusReci5 = [[_,2]*, [1,_]%] +

