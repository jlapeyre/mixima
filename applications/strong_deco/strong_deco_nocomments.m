
Permanent[m_List] := 
  With[{v = Array[x, Length[m]]}, Coefficient[Times @@ (m . v), Times @@ v]];


zzz[a_, b_] := a^b;

Perm[n_, q_] := 
  Module[{nn = n}, Permanent[Outer[zzz, Table[z[i], {i, 1, n}], 
     Table[q[[i]], {i, 1, n}]]]];

Slat[n_, q_] := 
  Module[{nn = n}, Det[Outer[zzz, Table[z[i], {i, 1, n}], 
     Table[q[[i]], {i, 1, n}]]]];


cc[0]=0;

tab[n_, l_] := Table[{cc[i], cc[i - 1], 
    If[i == 1, l, (l - Sum[cc[j], {j, 0, i - 1}])/2]}, {i, 1, n - 1}];

Pots[na_, L_] := 
  If[na == 2, Table[{i, L - i}, {i, 0, L/2}], 
   Module[{nna = na, nn = L}, Clear[pat]; 
     pat[nna] = 
      Join[Table[cc[i], {i, 1, nna - 1}], {nn - Sum[cc[i], {i, 1, nna - 1}]}];
      pat[a_] := Table[pat[a + 1], Evaluate[tab[nna, L][[a]]]]; 
     Flatten[pat[1], nna - 2]]];

     
ConjS[na_, L_] := 
  Module[{nna = na, nn = L}, poty = Pots[nna, nn]; 
    dimy = Dimensions[poty][[1]]; Table[Perm[nna, poty[[i]]], {i, 1, dimy}]];


PotsF[na_, L_] := 
  Module[{nna = na, nn = L}, Complement[Table[If[NumberQ[ConjS2[nna, nn][[i]]],
  0, Pots[nna, nn][[i]]], {i, 1, Dimensions[Pots[nna, nn]][[1]]}], {0}]];


ConjS2[na_, L_] := 
  Module[{nna = na, nn = L}, poty = Pots[nna, nn]; 
    dimy = Dimensions[poty][[1]]; Table[Slat[nna, poty[[i]]], {i, 1, dimy}]];


Laughlin[n_,nu_]:=Product[(z[i]-z[j])^(1/nu),{i,1,n},{j,i+1,n}];


Laug[n_]:=Laughlin[n,1/2];


Pfaffian[nn_, ff_] := 
  If[EvenQ[nn], Module[{n = nn}, 
    listy = Table[i, {i, 1, n}]; ab = Permutations[listy]; 
     sigs = SignaturePermutation /@ Permutations[listy]; 
     toli = Sum[sigs[[i]]*Product[1/(z[ab[[i,j]]] - z[ab[[i,j + 1]]]), 
          {j, 1, n, 2}], {i, 1, Dimensions[sigs][[1]]}]/(2^(n/2)*(n/2)!); 
     aout = toli*Laughlin[n, ff]; Expand[Simplify[aout]]], 0];

Conje[nn_] := If[EvenQ[nn], Module[{n = nn}, 
    listy = Table[i, {i, 1, n}]; ab = Permutations[listy]; 
     sigs = SignaturePermutation /@ Permutations[listy]; 
     toli = Sum[(Laughlin[n/2, 1/2] /. 
           Table[z[jj] -> z[ab[[i,jj]]], {jj, 1, n/2}])*
         (Laughlin[n/2, 1/2] /. 
           Table[z[jj - n/2] -> z[ab[[i,jj]]], {jj, n/2, n}]), 
        {i, 1, Dimensions[sigs][[1]]}]/(2^(n/2)*(n/2)!); aout = toli; aout], 
   Module[{n = nn}, listy = Table[i, {i, 1, n}]; ab = Permutations[listy]; 
     sigs = SignaturePermutation /@ Permutations[listy]; 
     toli = Sum[(Laug[(n + 1)/2] /. 
           Table[z[jj] -> z[ab[[i,jj]]], {jj, 1, (n + 1)/2}])*
         (Laug[(n - 1)/2] /. 
           Table[z[jj - (n + 1)/2] -> z[ab[[i,jj]]], 
            {jj, (n + 1)/2 + 1, n}]), {i, 1, Dimensions[sigs][[1]]}]/
       (2^(n/2)*(n/2)!); aout = toli; aout]];


deltaL2[n_,i_]:= Laug[n] ConjS[n,2][[i]];


deltaL4[n_, i_] := Laug[n] ConjS[n,4][[i]];


qh[n_,nu_,xi]:=Product[(xi-z[i]),{i,1,n}]Laughlin[n,nu];

quah[n_,nu_]:= qh[n,nu,0];


qp[na_, nu_, xi] := 
  Module[{nn = na}, dd[i_] := xi*dd[i - 1] - D[dd[i - 1], z[i]]; 
    dd[0] = Laughlin[nn, nu]; dd[nn]];


quap[n_]:= qp[n,1/2,0];

deltaQP2[n_,i_]:= quap[n] ConjS[n,2][[i]];


edge[n_]:=Laug[n]ConjS[n,1][[1]];


deltaP2[n_,i_]:= Conje[n] ConjS[n,2][[i]];

deltaP4[n_, i_] := Conje[n] ConjS[n,4][[i]];

nami[na_, L_] := 
  Module[{nna = na, nn = L}, potty = Pots[nna, nn]; 
    pp = Dimensions[potty][[1]]; 
    inde = Table[Complement[potty[[i]]], {i, 1, pp}]; 
    ta = Table[Table[Count[potty[[i]], inde[[i,j]]], 
       {j, 1, Dimensions[inde[[i]]][[1]]}], {i, 1, pp}]; 
    Table[Product[ta[[i,j]]!, {j, 1, Dimensions[ta[[i]]][[1]]}], {i, 1, pp}]];

tip[na_, L_] := 
  Module[{nna = na, nn = L}, potty = Pots[nna, nn]; nimy = nami[nna, nn]; 
    Table[Sqrt[nimy[[i]]]*Sqrt[Product[Pi*Gamma[potty[[i,jj]] + 1], 
        {jj, 1, nna}]], {i, 1, Dimensions[nimy][[1]]}]];


namiF[na_, L_] := 
  Module[{nna = na, nn = L}, potty = PotsF[nna, nn]; 
    pp = Dimensions[potty][[1]]; Table[1, {i, 1, pp}]];

tipF[na_, L_] := 
  Module[{nna = na, nn = L}, potty = PotsF[nna, nn]; nimy = namiF[nna, nn]; 
    Table[Sqrt[nimy[[i]]]*Sqrt[Product[Pi*Gamma[potty[[i,jj]] + 1], 
        {jj, 1, nna}]], {i, 1, Dimensions[nimy][[1]]}]];

LaugDeco[na_] := 
  Module[{nna = na}, state = Laug[nna]; base = ConjS[nna, nna*(nna - 1)]; 
    symb = Table[z[i], {i, 1, nna}]; 
    laur = PolynomialReduce[state, base, symb]; 
    If[laur[[2]] != 0, Print["Problem in reduction"]]; 
    prf = laur[[1]]*tip[nna, nna*(nna - 1)]; outp = prf/Sqrt[prf . prf]; 
    {outp}];

(*
LaugDeco[3]
LaugDeco[4]
LaugDeco[5]
*)
