Conjugate[1 + I]
Pi/2 * I * E
Conjugate[Exp[I Pi/4]]
Root[5 + 3 #^2 + 5 #^6 &, 4]
Conjugate[%]
{N[%], N[%%]}
Conjugate[2 - 1.6 I]
Conjugate[2 - 1.60000000000000000000000 I]
Conjugate[{-1 - I, 0, 1 + I}]
Conjugate[I Infinity]
Conjugate[ComplexInfinity]
toCCForm[f_, {x_, y_}, z_] :=  f /. {x -> (z + Conjugate[z])/2, y -> (z - Conjugate[z])/(2 I)}

