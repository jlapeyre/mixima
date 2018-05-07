Log[1000.]

Log[10, 1000]

Series[Log[1 + x], {x, 0, 10}]

Log[2.5 + I]

N[Log[5/2], 50]

Log[2.50000000000000000000000000000000000000000000]

Log[{2.1, 3.1, 4.1}]

Log[E]

Log[2, 1024]

Log[3, 3^-12]

Log[Pi, Sqrt[Pi]]

ComplexExpand[Log[x + I y]]

Log[0]

Log[ComplexInfinity]

NumericQ[Log[1 + E]]

Table[{n, Log[10, 1 + 1./n]}, {n, 1, 9}]

seq = NestList[3/2 (# + Mod[#, 2]) &, 1, 10000];
Take[seq, 15]

Sort[Tally[IntegerDigits[seq][[All, 1]]]]

h[p_List] := -p.Log[p]

Exp[Log[z]]

Log[Exp[z]]

PowerExpand[%]

FourierTransform[Log[t], t, s]

