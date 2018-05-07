(*qinf/qinfprg/qinf050.ma 12/29/05 

	CMU Quantum Information Programs in Mathematica.  
	File header contains abbreviations, alphabetical list of all objects,
lists of objects by categories.
	Main file contains definitions of objects


 ABBREVIATIONS
 ALPHABETICAL LIST OF OBJECTS

	LISTS BY CATEGORIES:
 BELL STATES
 GENERAL (not in some other category)
 HELPER FUNCTIONS
 INPUT/OUTPUT FUNCTIONS
 KETS, PROJECTORS, GATES, CODES
 PARTIAL TRACE, PARTIAL TRANSPOSE
 PAULI REPRESENTATION, SIGMA MATRICES
 RANDOM
 SCHMIDT DECOMPOSITION
 TENSOR PRODUCTS AND CONVERSIONS

		*ABBREVIATIONS
 ddl = double dimension list 
 dl = dimension list
 kt = ket (vector or list of numbers or symbols)
 ktn = ket as a tensor, k-tensor
 ls = list
 mt = matrix 
 ntn = normal or n-tensor
 ob = orthonormal basis 
 op = operator (in matrix form)
 orbasis = orthonormal basis
 otn = o-tensor ("dyad")
 ox = otimes = tensor product operator
 pm = list representing a permutation
 ptn = Pauli coefficient tensor
 tn = tensor
 W = general matrix (often unitary)

		*ALPHABETICAL LIST OF OBJECTS
addidents[]: Helper function for expandout[]
adjoint[mt]: Adjoint of matrix.
adjointc[mt]: ComplexExpand[ adjoint[mt]]
adjointr[mt]: Adjoint of real matrix = Transpose
basbell: Conjugate of Bell basis
bassbell: Conjugate of special Bell basis
bell[j]: =j'th Bell state for j=0,1,2,3. (2 defs available)
bellbas: Transpose of Bell basis
bell2mat[mt]: Converts matrix from Bell to regular basis
bin2ket[ls]: List ls of n 0's, 1's converted to ket in space 2^n
bket[bin,n]: Basis ket corresponding to 'bin' for n qubits
blochket[{x,y,z}]: ket corresponding to point on Bloch sphere

cgate[W]: Produces controlled-W gate from W
coeffs[kt,ob]: Expansion coefficients of ket kt in orthonormal basis ob
cnot: controlled-not gate with first qubit the control
copygate[W,n]: Matrix representing tensor product of W with itself n times
cphase: controlled-phse gate for two qubits

diags[mt]: diagonal elements of mt -> list (Inverse of DiagonalMatrix[ls])
dyad[kta,ktb]: Forms |kta><ktb| from the two kets
dyadc[kta,ktb]: Like dyad[], but uses ComplexExpand[Conjugate[ktb]]
dyadr[kta,ktb]: Forms |kta><ktb| if kta and ktb are real. (No Conjugate[])
dyap[kt]: Forms |kt><kt|; is same as dyad[kt,kt]

entang[kt,dl]:  Entanglement in bits for ket on tensor product; dl={da,db}
entsq[kt,dl]: Renyi entanglement: -log_2(trace of square of partial trace)
entropy[ls]: Finds the entropy (log base 2) for list ls of probabilities
exchg: Exchanges two qubits
expandout[op,ls,dl] Matrix on tensor product representing operator op
expandout2[op,ls,n] =expandout[] for n qubits

fivecode: Five qubit code
fourierg[n]: n x n unitary matrix for quantum Fourier transform
fouriern[kt]: Numerical quantum fourier transform
grschm[kts]: Gram-Schmidt: list {kets} -> orthonormal list
grschmr[kts]: Gram-Schmidt for REAL {kets}
hgate: Hadamard gate for 1 qubit
ident[n] = IdentityMatrix[n]
invertlist[]: Helper function for expandout[]
invperm[pm]: Returns inverse permutation to pm (list of 1 to n in some order)

ketcofs[kt,ob,dl] Expansion coefs of ket kt in orbasis ob of B in BxCx...
ketinner[kta,ktb]: Inner product <kta|ktb>
ketinnerc[kta,ktb]: ComplexExpand applied to ketinner
ketnorm[kt]: Normalizes ket kt
ketnormr[kt]: Normalizes real ket kt
ketprod[kt1,kt2,...]: tensor product kt1 ox kt2 ox ... as a single ket (list)
ket2bin[kt]: ket for qubits as list of coefs times |101>, etc. 
ket2kten[kt,dl]: Converts ket kt to tensor corresponding to list dl
ket2kten2[kt]: Converts ket kt to tensor of qubits
kten2ket[ktn]: Inverse of ket2kten and ket2kten2

matinner[mta,mtb]: =Tr[adjoint[mta].mtb], but faster. (Inner prod of mta,mtb.)
matinp[mta,mtb]: =Tr[ mta.mtb ], but faster.
matinq[mta,mtb]: Sum_{jk}  mta[[j,k]]*mtb[[j,k]]
matnorm[mt]: Normalizes rows of matrix mt
mat2bell[mt]: Converts matrix to Bell basis
mat2nten[mt,ddl]: Converts matrix mt to n-tensor using (double) list ddl
mat2nten2[mt]: Converts matrix mt on tensor product of qubits  to n-tensor
mat2oten[mt,dl]  Converts matrix to o-tensor for list dl
mat2oten2[mt]: Converts mt on qubits to o-tensor
mat2paul[mt]: Converts matrix to Pauli coefficient tensor
mat2sbell[mt] Converts matrix mat to special Bell basis

(*

ninecode: Nine qubit Shor code
nten2mat[ntn]: Converts n-tensor to (possibly rectangular) matrix
nten2oten[nten]: Converts n-tensor (normal) to o-tensor (dyad).
oten2mat[oten]: Converts dyad-form tensor oten to matrix
oten2nten[t]: Tensor operator from dyad to normal convention
oten2paul[oten]: Converts o-tensor to Pauli representation tensor
outer[tn1, tn2, ...]: Outer product of tn1, tn2, ... = Outer[Times,tn1,tn2 ..]

*)

partrace[mt,q,dl]: Partial trace of mt over q'th factor in tensor product dl
partrace2[mt,q]:  Partial trace of mt over q'th qubit
partrans[mt,q,dl]: Partial transpose of mt over space q (1,2,...) in product dl
partrans2[mt,q]:  Partial transpose of mt over qubit q in tensor product
paulnz[ptn]: Helper for prtpaul. Extracts nonzero elements of ptn
paulnzch[ptn,ep]: Helper for prtpaulch. Finds nonzero elements of Chop[ptn,ep]
paulten[args]: paulten[0,1,3] is Pauli tensor I ox sg_x ox sg_z
paul2oten[ptn]:  Converts Pauli coeff tensor to o-tensor
paul2mat[ptn]:  Converts Pauli coeff tensor to matrix

permket[kt,pm,dl]: Transforms ket kt to permuted order pm of tensor product dl
permket2[kt,pm]:   Transforms ket kt on qubit to permuted order pm of qubits
permmat[mt,pm,dl]: Transforms mt to permuted order pm of tensor product dl
permmat2[mt,pm]: Converts matrix mt according to permutation p of qubits
permptrace: Helper function for partialtrace
permno: Helper function for nten2oten
permon: Helper function for oten2nten
permtrans: Helper function for partial transpose
permute[ls,pm]: Permutes list ls according to permutaton pm
permutmat[pm]: Permutation matrix corresponding to permutation pm as list
plabc[ls]: Helper for paulten. Generates labels c[...]=
pop2dop[mt]: Divides mt by Tr[mt] to make a density operator (if mt positive)
prodlist[ls]: Product of elements in list ls
prtpaul[ptn]: Labels and prints nonzero elements of Pauli tensor ptn
prtpaulch[ptn,ep]: Labels and prints nonzero elements of Chop[ptn,ep]

quadn[ml]: Sum of absolute squares of elements of ml: vector,matrix,tensor
quadr[ml]: Version of quadn assming ml is real

ranbas[n]: Random orbasis for n-dimensional Hilbert space
ranbasr[n]: Random real orbasis for n-dimensional Hilbert space
ranbell: Random fully-entangled orbasis for 2 qubits as list of 4 kets
ranket[n]: Random ket on n-dimensional space
ranketr[n]: Random real ket on n-dimensinal space
ranorn[m,n]: Random collection of m orthonormal kets on n-dimens space
ranornr[m,n]: Random collection of m real normalized kets on n-dimens space
rgate[j,th]: Single qubit R_j(th) gate

sbell[j]: j'th special Bell state
sbellbas: Converts ket from special Bell to regular basis
sbell2mat[mt]: Converts matrix mt from special Bell basis to standard basis

schmidt[kt,dl]: Returns Schmidt decomposition of normalized kt as a list
schmidtprobs[kt,dl]: Returns list of Schmidt probabilities for ket
schmidtproj[ls]: Constructs a projector on subspace of Schmidt basis
schmidt2ket[ls]: Inverse of schmidt[]

sevencode: Seven qubit Steane code
sig[j]: Sigma matrix on one qubit for j=0,1,2,3. sig[0]=I
sigl[ls]: Tensor product of Pauli operators in list ls
sigprod[j,k,...]: Tensor product sig[j] ox sig[k] ox ...
sumlist[ls]: Returns sum of items in list ls

tenprod[mt1,mt2,...]: Tensor product of matrices mt1, mt2,... as a matrix
threecode: Three qubit code that corrects bit-flip errors
traceout[mt,ls,dl]  Partial trace of mt over spaces in list ls
traceout2[mt,ls] Partial trace of mt over qubits in list ls
transpose[]: Same as Transpose[], but returns a ket unchanged

xgate: Single qubit X gate
xket[m]: xket[0], xket[1] are kets for +x, -x axis points on Bloch sphere
xprj[m]: For m=0, 1 projects on +x, -x states of Bloch sphere
ygate: Single qubit Y gate
yket[m]: yket[0], yket[1] are kets for +y, -y axis points on Bloch sphere
yprj[m]: For m=0, 1 projects on +y, -y states of Bloch sphere
zgate: Single qubit Z gate
zket[m]: zket[0]=|0>, zket[1]=|1>: kets for +z, -z axis points on Bloch sphere
zprj[m]: =|m><m| for m=0, 1: projects on +z, -z states of Bloch sphere
		END ALPHABETICAL LIST OF OBJECTS

		*BELL STATES
basbell: Conjugate of Bell basis
bassbell: Conjugate of special Bell basis
bell[j]: =j'th Bell state for j=0,1,2,3. (2 defs available)
bellbas: Transpose of Bell basis
bell2mat[mt]: Converts matrix from Bell to regular basis
mat2bell[mt]: Converts matrix to Bell basis
mat2sbell[mt] Converts matrix mat to special Bell basis
ranbell: Random fully-entangled orbasis for 2 qubits as list of 4 kets
sbell[j]: j'th special Bell state
sbellbas: Converts ket from special Bell to regular basis
sbell2mat[mt] Converts matrix mt from special Bell basis to standard basis



		*GENERAL (not in some other category)
adjoint[mt]: Adjoint of matrix.
adjointc[mt]: ComplexExpand[ adjoint[mt]]
adjointr[mt]: Adjoint of real matrix = Transpose
coeffs[k,ob]: Expansion coefficients of ket k in orthonormal basis ob
diags[mt]: diagonal elements of mt -> list (Inverse of DiagonalMatrix[ls])
dyad[kta,ktb]: Forms |kta><ktb| from the two kets
dyadc[kta,ktb]: Like dyad[], but uses ComplexExpand[Conjugate[ktb]]
dyadr[kta,ktb]: Forms |kta><ktb| if kta and ktb are real. (No Conjugate[])
dyap[kt]: Forms |kt><kt|; is same as dyad[kt,kt]

entang[kt,dl]  Entanglement in bits for ket on tensor product; dl={da,db}
entropy[ls]: Finds the entropy (log base 2) for list ls of probabilities
entsq[kt,dl]: Renyi entanglement: -log_2(trace of square of partial trace)
grschm[kets]: Gram-Schmidt: list {kets} -> orthonormal list
grschmr[kets]: Gram-Schmidt for REAL {kets}
invperm[pm]: Returns inverse permutation to pm (list of 1 to n in some order)
ketcofs[kt,ob,dl] Expansion coefs of ket kt in orbasis ob of B in BxCx...
ketinner[kta,ktb]: Inner product <kta|ktb>
ketinnerc[kta,ktb]: ComplexExpand applied to ketinner
ketnorm[kt]: Normalizes ket kt
ketnormr[kt]: Normalizes real ket kt
ket2bin[kt]: ket for qubits as list of coefs times |101>, etc. 

matinner[mta,mtb]: =Tr[adjoint[mta].mtb], but faster. (Inner prod of mta,mtb.)
matinp[mta,mtb]: =Tr[ mta.mtb ], but faster.
matinq[mta,mtb]: Sum_{jk}  mta[[j,k]]*mtb[[j,k]]
matnorm[mt]: Normalizes rows of matrix mt
outer[tn1, tn2, ...]: Outer product of tn1, tn2, ... = Outer[Times,tn1,tn2 ..]
permute[ls,pm]: Permutes list ls according to permutaton pm
permutmat[p]: Permutation matrix corresponding to permutation p as list
pop2dop[mt]: Divides mt by Tr[mt] to make a density operator (if mt positive)
prodlist[ls]: Product of elements in list ls
quadn[ml]: Sum of absolute squares of elements of ml: vector,matrix,tensor
quadr[ml]: Version of quadn assming ml is real
sumlist[ls]: Returns sum of items in list ls
transpose[]: Same as Transpose[], but returns a ket unchanged
		*END GENERAL

		*HELPER FUNCTIONS
addidents[]: Helper function for expandout[]
invertlist[]: Helper function for expandout[]
paulnz[ptn]: Helper for prtpaul 
paulnzch[ptn,ep]: Helper for prtpaulch
permptrace: Helper function for partialtrace
permno: Helper function for nten2oten
permon: Helper function for oten2nten
permtrans: Helper function for partial transpose
plabc[ls]: Helper for paulten

		*INPUT/OUTPUT FUNCTIONS
bin2ket[ls]: List ls of n 0's, 1's converted to ket in space 2^n
bket[bin,n]: Basis ket corresponding to 'bin' for n qubits; see ket2bin[]
blochket[{x,y,z}]: ket corresponding to point on Bloch sphere
ket2bin[kt]: ket for qubits as list of coefs times |101>, etc.
paulten[args]: paulten[0,1,3] is Pauli tensor I ox sg_x ox sg_z
prtpaul[ptn]: Labels and prints nonzero elements of Pauli tensor ptn
prtpaulch[ptn,ep]: Labels and prints nonzero elements of Chop[ptn,ep]

		*KETS, PROJECTORS, GATES, CODES
cgate[W]: Produces controlled-W gate from W
cnot: controlled-not gate with first qubit the control
copygate[W,n]: Matrix representing tensor product of W with itself n times
cphase: controlled-phse gate for two qubits
exchg: Exchanges two qubits
fivecode: Five qubit code
fourierg[n]: n x n unitary matrix for quantum Fourier transform
fouriern[kt]: Numerical quantum fourier transform
hgate: Hadamard gate for 1 qubit
ident[n] = IdentityMatrix[n]
ninecode: Nine qubit Shor code
rgate[j,th]: Single qubit R_j(th) gate
sevencode: Seven qubit Steane code
threecode: Three qubit code that corrects bit-flip errors
xgate: Single qubit X gate
xket[m]: xket[0], xket[1] are kets for +x, -x axis points on Bloch sphere
xprj[m]: For m=0, 1 projects on +x, -x states of Bloch sphere
ygate: Single qubit Y gate
yket[m]: yket[0], yket[1] are kets for +y, -y axis points on Bloch sphere
yprj[m]: For m=0, 1 projects on +y, -y states of Bloch sphere
zgate: Single qubit Z gate
zket[m]: zket[0]=|0>, zket[1]=|1>: kets for +z, -z axis points on Bloch sphere
zprj[m]: =|m><m| for m=0, 1: projects on +z, -z states of Bloch sphere

		*PARTIAL TRACE, PARTIAL TRANSPOSE
traceout[mt,ls,dl]  Partial trace of mt over spaces in list ls
traceout2[mt,ls] Partial trace of mt over qubits in list ls
partrace[mt,q,dl]: Partial trace of mt over q'th factor in tensor product dl
partrace2[mt,q]:  Partial trace of mt over q'th qubit
partrans[mt,q,dl]: Partial transpose of mt over space q (1,2,...) in product dl
partrans2[mt,q]:  Partial transpose of mt over qubit q in tensor product

		*PAULI REPRESENTATION, SIGMA MATRICES
mat2paul[mt]: Converts matrix to Pauli coefficient tensor
oten2paul[oten]: Converts o-tensor to Pauli representation tensor
paul2oten[ptn]  Converts Pauli coeff tensor to o-tensor
paul2mat[ptn]  Converts Pauli coeff tensor to matrix
paulten[args]: paulten[0,1,3] is Pauli tensor I ox sg_x ox sg_z
paulnz[ptn]: Helper for prtpaul. Extracts nonzero elements of ptn
paulnzch[ptn,ep]: Helper for prtpaulch. Finds nonzero elements of Chop[ptn,ep]
plabc[ls]: Helper for paulten. Generates labels c[...]=
plop[j1,j2,..]  sig[j1] ox sig[j2] ox ... as a matrix
prtpaul[ptn]: Labels and prints nonzero elements of Pauli tensor ptn
prtpaulch[ptn,ep]: Labels and prints nonzero elements of Chop[ptn,ep]

sig[j]: Sigma matrix on one qubit for j=0,1,2,3. sig[0]=I
sigl[ls]: Tensor product of Pauli operators in list ls
sigprod[j,k,...]: Tensor product sig[j] ox sig[k] ox ...
		*END PAULI REPRESENTATION, SIGMA MATRICES

		*RANDOM
ranbas[n]: Random orbasis for n-dimensional Hilbert space
ranbasr[n]: Random real orbasis for n-dimensional Hilbert space
ranbell: Random fully-entangled basis for 2 qubits as list of 4 kets
ranket[n]: Random ket on n-dimensional space
ranketr[n]: Random real ket on n-dimensinal space
ranorn[m,n]: Random collection of m orthonormal kets on n-dimens space
ranornr[m,n]: Random collection of m real normalized kets on n-dimens space

		*SCHMIDT DECOMPOSITION
schmidt[kt,dl]: Returns Schmidt decomposition of normalized kt as a list
schmidtprobs[kt,dl]: Returns list of Schmidt probabilities for ket
schmidtproj[ls]: Constructs a projector on subspace of Schmidt basis
schmidt2ket[ls]: Inverse of schmidt[]

		*TENSOR PRODUCTS AND CONVERSIONS
expandout[op,ls,dl] Matrix on tensor product representing operator op
expandout2[op,ls,n] =expandout[] for n qubits
ketprod[kt1,kt2,...]: tensor product kt1 ox kt2 ox ... as a single ket (list)
ket2kten[kt,dl]: Converts ket kt to tensor corresponding to list dl
ket2kten2[kt]: Converts ket kt to tensor of qubits
kten2ket[kt]: Inverse of ket2kten and ket2kten2
mat2nten[mt,ddl]: Converts matrix mt to n-tensor using (double) list ddl

mat2oten[mt,dl]  Converts matrix to o-tensor for list dl
mat2oten2[mt]: Converts mt on qubits to o-tensor
mat2paul[mt]: Converts matrix to Pauli coefficient tensor
mat2nten2[mt]: Converts matrix mt on tensor product of qubits to n-tenso
nten2mat[ntn]: Converts n-tensor to (possibly rectangular) matrix
nten2oten[nten]: Converts n-tensor (normal) to o-tensor (dyad).
oten2mat[oten]: Converts dyad-form tensor oten to matrix
oten2nten[t]: Tensor operator from dyad to normal convention
oten2paul[oten]: Converts o-tensor to Pauli representation tensor
outer[T1,T2,...]: Outer product of T1, T2, ... (=Outer[Times,T1,T2,...])
paul2oten[ptn]:  Converts Pauli coeff tensor to o-tensor
paul2mat[ptn]:  Converts Pauli coeff tensor to matrix
permket[kt,pm,dl: Transforms ket kt to permuted order pm of tensor product dl
permket2[kt,pm]:   Transforms ket kt on qubit to permuted order pm of qubits
permmat[mt,pm,dl]: Transforms mt to permuted order p of tensor product dl
sigl[ls]: Tensor product of Pauli operators in list ls
sigprod[j,k,...]: Tensor product sig[j] ox sig[k] ox ...
tenprod[mt1,mt2,...]: Tensor product of matrices mt1, mt2,... as a matrix
		*END TENSOR PRODUCTS AND CONVERSIONS

*)

		(*BEGIN OBJECTS*)

Needs["Statistics`NormalDistribution`"] (*For random functions*)

		addidents::usage = "tensor n one qubit identity operators to
the right of op";
	addidents[op_,n_]:=If[n==0,op,addidents[tenprod[op,sig[0]],n-1]];

		adjoint::usage = "adjoint[mat] is the complex conjugate of the
transpose. For a ket this is simply the complex conjugate."
	adjoint[mat_]:=transpose[Conjugate[mat]]

		adjointc::usage = "adjointc[mat] applies ComplexExpand to
adjoint[]"
	adjointc[mat_]:= ComplexExpand[Conjugate[transpose[mat]]];

		adjointr::usage = "adjointr[mat] is just the transpose; it is
the adjoint if mat is real"
	adjointr[mat_]:=transpose[mat]

(*basbell -> following bellbas*)
(*bassbell -> following sbellbas*)

		(*Bell basis, Version 1*)
bell[0] = {1,0,0,1}/Sqrt[2]; 
bell[1] = {0,1,1,0}/Sqrt[2];
bell[2] = {1,0,0,-1}/Sqrt[2]; 
bell[3] = {0,1,-1,0}/Sqrt[2];

		(*Bell basis, Version 2*)
bell[0] = {1,0,0,1}/Sqrt[2]; 
bell[1] = {1,0,0,-1}/Sqrt[2]; 
bell[2] = {0,1,1,0}/Sqrt[2];
bell[3] = {0,1,-1,0}/Sqrt[2];

		(*Also see special Bell basis, 'sbell'*)

bellbas=transpose[Table[bell[j],{j,0,3}]]; (* bellbas . bellket = ket *) 
basbell=adjoint[bellbas]; (* basbell . ket = ket in Bell basis *)
bell2mat[mat_]:= bellbas . mat . basbell
(*And see 'mat2bell'*)

		bin2ket::usage = "bin2ket[ls] takes list ls of n 0's and 1's,
thought of as basis states of n qubits, and returns the corresponding ket in a
2^n dimensional Hilbert space. E.g. bin2ket[{0,1}] -> {0,1,0,0}.  One can
multiply by coefficients, and add if the number of qubits is the
same. c*bin2ket[{0,1}] + d*bin2ket[{1,1}] -> {0,c,0,d}.  Also see bket[]."
	bin2ket[ls_]:= Module[{ket,ln=Length[ls],m},
ket=Table[0,{2^ln}];
m=1+Fold[2*#1+#2&,0,ls];
++ket[[m]]; 
ket](*END bin2ket*)

		bket::usage = "bket[bin,n]. Returns stadard form of basis ket
corresponding to |bin>, where 'bin' is a string of n 0's or 1's. E.g.,
bket[01,2]={0,1,0,0}; bket[010,3]={0,0,1,0,0,0,0,0}. Also see bin2ket"
	(*bket:comment. Due to difficulty in getting Mca to interpret
'000' as different from '0', this function adds 2*10^n to the n-bit 'bin'
and converts the result to a string, which is then converted back to a list 
by Characters[] followed by ToExpression.  Finally, the 2 is discarded using 
Take[], and the result converted to the corresponding binary number using
Fold[]. Adding 1 to the result yields m, the position where the list
representing the starndard ket is changed from 0 to 1.*)
	bket[bin_,n_]:=Module[{lst=Table[0,{2^n}],m},
m=1+Fold[2*#1+#2&,0,
   Take[ToExpression[   Characters[ ToString[2*10^n+bin] ]   ],-n]];
lst[[m]]=1;lst] (*END bket*)

		blochket::usage = "blochket[{x,y,z}] takes the Cartesian
coordinates of a point on the Bloch sphere and returns the corresponding ket in
the form {cos(th/2),sin(th/2)e^i*phi}."
	(*blochket: The If[] is intended to suppress an error message from
ArcTan[] if both x and y are 0.*)
	blochket[ls_]:= Module[{theta,phi,x=ls[[1]],y=ls[[2]],z=ls[[3]]},
theta = ArcCos[z];
If[ ((0==x)&&(0==y))||(0.==x)&&(0.==y), phi=0, phi = ArcTan[x,y],
phi = ArcTan[x,y]];
{Cos[theta/2], Sin[theta/2]*E^(I*phi)}]

		cgate::usage = "cgate[W_] returns a controlled-W on A x B,
where A is the control qubit and W a unitary on B (any dimension), as a matrix"
	cgate[w_]:= tenprod[{{1,0},{0,0}},IdentityMatrix[Length[w]]] + 
tenprod[{{0,0},{0,1}},w]

		cnot::usage = "returns controlled-not gate on 2 qubits, with
first qubit the control"
	cnot = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 0, 1}, {0, 0, 1, 0}}

		coeffs::usage = "coeffs[v,b] gives the list of expansion
coefficients of the ket v in the orthonormal basis b (= list of basis
vectors)."
	coeffs[v_,b_]:= Conjugate[b] . v ;

		copygate::usage = "copygate[gate,nn] returns the tensor product
gate ox gate ox ... ox gate, containing 'gate' nn times, as a matrix"
	copygate[gate_,nn_]:=Module[{fgate=gate,jn},
For[jn=2,jn<=nn,++jn, fgate = tenprod[fgate,gate] ];fgate](*END copygate*)

		cphase::usage = "returns controlled-phase gate on two qubits"
	cphase = {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, -1}}

		diags::usage = "diags[M] takes a matrix M as a list of lists,
and extracts the diagonal elements as a single list."
	(*diags:comment.  This is the inverse to Mca DiagonalMatrix*)
	diags[mat_] := Module[{j,v=Table[0,{l=Length[mat]}]},
v=Table[0,{l}]; For[j=1,j<=l,++j, v[[j]] = mat[[j,j]] ]; v]

		dyad::usage = "Takes |a> |b>, represented as lists, and forms
the matrix |a><b|, applying Conjugate to |b>."
	dyad[a_,b_]:= Outer[Times,a,Conjugate[b]];

		dyadc::usage = "Takes |a> |b>, represented as lists, and forms
the matrix |a><b|, applying ComplexExpand[Conjugate to |b>."
	dyadc[a_,b_]:= Outer[Times,a,ComplexExpand[Conjugate[b]]];

		dyadr::usage = "Takes two kets, each REAL and represented by a
list, and forms the corresponding dyad matrix.  |a>, |b> -> |a><b|."
	dyadr[a_,b_]:= Outer[Times,a,b];

		dyap::usage = "dyap[kt_] makes operator |kt><kt| from ket kt."
	dyap[kt_]:=dyad[kt,kt]

		entang::usage = "entang[ket,dl] takes a ket on a tensor product
AxB, with dl = {dim A, dim B}, normalizes it, forms the partial trace of the
projector, and uses this to compute the entanglement in bits."
	entang[ket_,dl_]:= Module[{eps = 10^-16,evals,j,rho,rhoa,sum,x},
rho=dyad[ket,ket];
rhoa = Chop[ partrace[rho,2,dl]/Tr[rho] ];
evals = Re[ Eigenvalues[rhoa] ];
sum=0;
	For[j=1,j<=dl[[1]],++j,
x=evals[[j]]; 
If[x > eps, sum += x*Log[x]];
	];
-sum/Log[2] ]
(*END entang*)

		entsq::usage = "entsq[ket,dl] takes a ket on a tensor product
AxB, with dl = {dim A, dim B}, normalizes it, forms the partial trace rhoa of
the projector, and returns -log_2 of the trace of its square, for a Renyi
entanglement."
	entsq[ket_,dl_] := Module[{rho,rhoa},
rho=dyad[ket,ket];
rhoa = Chop[ partrace[rho,2,dl]/Tr[rho] ];
-Log[Re[ Tr[rhoa.rhoa] ]]/Log[2.]  ]
(*END entsq*)

		entropy::usage = "entropy[list] takes a list of probabilities
{p_i} and calculates the sum {-p_i log p_i}, where log is to base 2"
	(*entropy: A cutoff of 10^-12 has been inserted to avoid log(0) *)
	entropy[list_]:=Module[{j,n=Length[list],p,sum},
sum=0; 
	For[j=1,j<= n,++j,
p = list[[j]];
If[p < 10^-12,Continue[] ];
sum += p*Log[2,p];
	];
-sum] 
(*END entropy*)

		exchg::usage = " 4 x 4 unitary that exchanges two qubits"
	exchg = { {1,0,0,0},{0,0,1,0},{0,1,0,0},{0,0,0,1} }

		expandout::usage = "expandout[op,ls,dl] takes an operator op as
a matrix defined on a list ls of Hilbert spaces in the tensor product of spaces
with dimensions given by dl, and returns it as a matrix on the full space.
E.g., expandout[cnot,{3,2},{4,2,2}] gives a controlled not with the last qubit
(3rd space) the control."
	expandout[op_,ls_,dl_]:=   permmat[tenprod[op,IdentityMatrix[Fold[Times,1,dl]/Length[op]]],
      Join[ls,invertlist[Length[dl],Sort[ls]]],dl];

		expandout2::usage = "expandout2[op,ls,n] takes a matrix op
representing a gate or other operation, a list ls of the qubits which forms the
basis of the matrix, and the total number n of qubits in circuit, and forms the
2^n by 2^n matrix representing that operation";
	expandout2[op_,ls_,n_]:= permmat2[addidents[op,n-Log[2,Length[op]]],Join[ls,invertlist[n,Sort[ls]]]];

(*

		fivecode::usage = "{ |0_L>, |1_L>} for 5 qubit code"
fivecode = { 
  bket[00000,5] + bket[10010,5] +bket[01001,5] +bket[10100,5] +
   bket[01010,5] - bket[11011,5] -bket[00110,5] -bket[11000,5]
   -bket[11101,5] -bket[00011,5] -bket[11110,5] -bket[01111,5]
   -bket[10001,5] -bket[01100,5] -bket[10111,5] +bket[00101,5] ,
   +bket[11111,5] +bket[01101,5] +bket[10110,5] +bket[01011,5] 
   +bket[10101,5] -bket[00100,5] -bket[11001,5] -bket[00111,5] 
   -bket[00010,5] -bket[11100,5] -bket[00001,5] -bket[10000,5] 
   -bket[01110,5] -bket[10011,5] -bket[01000,5] +bket[11010,5]  }/4

*)

		fourierg::usage = "fourierg[n] produces a n x n
unitary matrix representing the quantum Fourier transform."
		fourierg[n_]:= Table[Exp[2*Pi*I*j*k/n]/Sqrt[n],
{j,0,n-1}, {k,0,n-1}]

		fouriern::usage = "fouriern[ket] produces ket' = QFT ket
using Mathematica Fourier[], where ket must be a string of (complex) numbers."
	fouriern[ket_]:=Fourier[ket]

		grschm::usage = "grschm[ls] produces from a list ls of kets an
orthonormal set.  The original set must be linearly independent."
(*grschm:comment.  Input list ls is indexed by j, output list indexed by k.*)
	grschm[ls_]:=Module[{j,k,ln=Length[ls],ns={},v,w},
		For[j=1,j<=ln,++j,
v=ls[[j]]; w=v;
	For[k=1,k<j,++k,
w = w - ketinner[ns[[k]],v]*ns[[k]];
	]; 
ns=Append[ns,ketnorm[w]]; 
		]; ns] (*END grschm*)

		grschmr::usage = "grschmr[ls] produces from a list ls of real
kets an orthonormal set.  The original set must be linearly independent."
(*grschm:comment.  Input list ls is indexed by j, output list indexed by k.*)
	grschmr[ls_]:=Module[{j,k,ln=Length[ls],ns={},v,w},
		For[j=1,j<=ln,++j,
v=ls[[j]]; w=v;
	For[k=1,k<j,++k,
w = w - (ns[[k]].v)*ns[[k]];
	]; 
ns=Append[ns,ketnormr[w]]; 
		]; ns] (*END grschm*)

		hgate::usage = "Hadamard gate for 1 qubit"
	hgate = { {1,1}, {1,-1} }/Sqrt[2]

		ident::usage = "ident[n]=IdentityMatrix[n]"
	ident[n_] := IdentityMatrix[n]

		invertlist::usage = "takes n, and a sorted list l, returns list
of elements not in list";
	invertlist[n_,l_]:=Complement[Array[#&,{n}],l]

		invperm::usage = "invperm[perm] returns inverse permutation
to perm, a list of integers 1 to n in some order."
	invperm[perm_]:=Module[{invp,j,ln=Length[perm]},
invp=Table[0,{ln}]; 
For[j=1,j<=ln,++j,invp[[ perm[[j]] ]]=j];invp](*END invperm*)
		

		ketcofs::usage = "ketcofs[v_,b_,dl_] returns a list of kets
which are the expansion coefficients of ket v in the orthonormal basis b (list
of basis vectors) of the first factor in a tensor product BC.... Here dl is the
list of dimensions of the factors, e.g., {3,4}, in which case b is a 3x3
matrix."
	(*Comment.  The Map[Flatten...] is needed in order that the
final output is a list of kets and not a list of tensors, in the case in which
dl contains more than two elements*)
	ketcofs[v_,b_,dl_]:= Map[Flatten,Conjugate[b] . ket2kten[v,dl]];

		ketinner::usage = "ketinner[v,w] =  inner product <v|w>";
	ketinner[v_, w_] := adjoint[v].w;

		ketinnerc::usage = "ketinnerc[v,w] = ComplexExpand applied to
inner product <v|w>";
	ketinnerc[v_, w_] := ComplexExpand[ adjoint[v].w ];
		

		ketnorm::usage = "ketnorm[v] returns the normalized counterpart
of the ket v."
	ketnorm[v_]:= v/Sqrt[Conjugate[v].v];

		ketnormr::usage = "ketnormr[v] returns the normalized
counterpart of the real ket v."
	ketnormr[v_]:= v/Sqrt[v.v];

		ketprod::usage = "ketprod[kt1,kt2,...] returns 
tensor product kt1 ox kt2 ox ... as a single ket (i.e., list)."
	ketprod[args__]:=Flatten[ outer[args] ]

		ket2bin::usage = "ket2bin[ket] assumes list of length 2^n
represents n-qubit k, and produces a list where each member of the ket list is
associated with a symbol of type, say |010>. E.g., ket = {al,0,bt,2} yields
{{al,|00>},{bt,|10>},{2,|11>}}."
	(*ket2bin:comment. It could undoubtedly be made more readable, but
this crude form has the advantage that the ket list can be either numerical, or
symbols, or a combination. If one had just numbers, replacing 0==item with
0.==Abs[Chop[item]] would be advantageous*)
	ket2bin[ket_]:=Module[
{it,item,jt,lng=Length[ket],nlist,nn,olist={},str},
nn=IntegerExponent[lng,2];
		For[it=0,it<lng,++it,
nlist = IntegerDigits[it,2,nn];
str="|";
For[jt=1,jt<=nn,++jt, str = str<>ToString[nlist[[jt]]]; ];
str = str<>">";
item = ket[[it+1]];
If[ 0==item, Continue[]];
AppendTo[olist,{item,str}] 
		]; olist](*END ket2bin*)

		ket2kten::usage = "ket2kten[ket, dl] transforms ket to a tensor
on the product space given by dl.  E.g., if dl={3,2}, a 6 component ket is
mapped to t_jk, with j in [1,3] and k in [1,2]"
	ket2kten[v_, dl_] :=     If[Length[dl] == 1, v, 
      Map[ket2kten[#, Rest[dl]] &, Partition[v, Length[v]/First[dl]]]];

		ket2kten2::usage = "ket2kten2[ket] transforms ket to a tensor
on a product space of qubits.  The dimension of ket must be 2^n."
	ket2kten2[ket_]:= Module[{va=ket}, 
While[ Length[va] > 2,va = Partition[va,2] ];va]

		kten2ket::usage = "Inverse of ket2kten";
	kten2ket[t_] := Flatten[t];

		mat2paul::usage = "mat2paul[mat] converts matrix for qubits
to Pauli representation tensor.  New name for mattopauli"
	mat2paul[mat_]:= oten2paul[mat2oten2[mat]]

		matinner::usage = "matinner[amat,bmat] computes the matrix
inner product Tr[adjoint[mata] . matb], but because it does not actually find
the matrix product it is faster."
	matinner[amat_,bmat_]:= Module[{cmat=Conjugate[amat],ln=Length[bmat]},
Sum[bmat[[j]].cmat[[j]],{j,ln}] ](*END matinner*)

		matinp::usage = "matinp[amat,bmat] evaluates Tr[amat . bmat]
without computing the full matrix product (which makes it faster)."
	matinp[amat_,bmat_]:= Module[{cmat=transpose[amat],ln=Length[bmat]},
Sum[bmat[[j]].cmat[[j]],{j,ln}] ](*END matinp*)

		matinq::usage = "matinq[amat,bmat]=sums amat[[j,k]]*bmat[[j,k]]
over j and k. Here amat must be a matrix, bmat could be a tensor of rank >2."
	matinq[amat_,bmat_]:= Module[{ln=Length[amat]},
Sum[amat[[j]].bmat[[j]],{j,ln}] ](*END matinq*)

		matnorm::usage = "matnorm[M] normalizes each row of the matrix
M."
	matnorm[mat_]:= Map[ketnorm,mat];

		mat2bell::usage = "mat2bell[mat] converts a 4 x 4 matrix mat to
the Bell basis"
	mat2bell[mat_]:= basbell . mat . bellbas

		mat2nten::usage = "mat2nten[mt,ddl] converts the (possibly
rectangular) matrix mt to an n-tensor using the double dimension list ddl,
with, e.g., {2,3,{4,5}} interpreted as {{2,2},{3,3},{4,5}}."
(*mat2nten:comment. Dimension list ddl is converted to ddm with every 
entry a two-component list, e.g.,  ddl={2,{3,4}}->ddm={{2,2},{3,4}}.*)
	mat2nten[mt_,ddl_]:=Module[
{ddm=ddl,fmt=Flatten[mt],jm,lnd=Length[ddl]},
		For[jm=1,jm<=lnd,++jm, 
If[  0==Length[ ddm[[jm]] ]  , ddm[[jm]]={ddl[[jm]],ddl[[jm]]}  ];  ];
Fold[ Partition, fmt, Most[Reverse[Flatten[Transpose[ddm]]]] ]](*END mat2nten*)

		mat2nten2::usage = "mat2nten2[mt] assumes mt is a
2^m x 2^m matrix for some integer m, and converts it to an n-tensor."
	mat2nten2[mt_]:=Module[{ntnm = Flatten[mt]},
While[ Length[ntnm] > 2, ntnm = Partition[ntnm,2] ]; ntnm]

		mat2oten::usage = "mat2oten[mt_,ddl_] converts the (possibly
rectangular) matrix mt to an o-tensor using the double dimension list ddl,
with, e.g., {2,3,{4,5}} interpreted as {{2,2},{3,3},{4,5}}."
	mat2oten[mt_,ddl_]:=nten2oten[mat2nten[mt,ddl]]

		mat2oten2::usage = "mat2oten2[mt] assumes mt is a
2^n x 2^n matrix for some integer n, and converts it to an o-tensor."
	mat2oten2[mt_]:=nten2oten[ mat2nten2[mt] ]

		mat2paul::usage = "mat2paul[mat] is the tensor
c[[j1,j2,...jn]] of coefficients of the expansion of the 2^n x 2^n matrix mat
in the form Sum c[[j1,j2,...jn]] sigma^1_j1 ... sigma^n_jn"
	mat2paul[mat_]:=oten2paul[mat2oten2[mat]]

		mat2sbell::usage = "mat2sbell[mat] converts a 4 x 4 matrix mat
to the special Bell basis"
	mat2sbell[mat_]:= bassbell . mat . sbellbas

(*

		ninecode::usage = "{ |0_L>, |1_L> } for Shor 9 qubit code"
	ninecode = Module[{shora,shorb},
shora = bket[000,3]+bket[111,3];
shorb = bket[000,3]-bket[111,3];
{ ketprod[shora,ketprod[shora,shora]],
 ketprod[shorb,ketprod[shorb,shorb]] }/Sqrt[8] ]

*)

		nten2mat::usage = "nten2mat[ntn] converts the n-tensor ntn to
a (possibly rectangular) matrix."
	nten2mat[ntn_]:=Module[{dims=Dimensions[ntn]},
Partition[ Flatten[ntn] , prodlist[Take[dims,-Length[dims]/2]] ]
	](*END nten2mat*)

		nten2oten::usage = "nten2oten[ntn] converts n-tensor referenced
(i,j,...,i',j',...) to an o-tensor referenced (i,i',j,j'...)."
	nten2oten[ntn_]:=transpose[ntn,permno[TensorRank[ntn]/2]];

		oten2mat::usage = "oten2mat[otn] converts o-tensor otn
to a (possibly rectangular) matrix."
	oten2mat[otn_] := nten2mat[oten2nten[otn]]

		oten2nten::usage = "oten2nten[otn] converts o-tensor 
referenced (i,i',j,j'...) to an n-tensor referenced (i,j,...,i',j',...)."
	oten2nten[otn_]:=transpose[otn,permon[TensorRank[otn]/2]];

		oten2paul::usage = "oten2paul[oten] returns the Pauli
coefficient tensor for an operator in the form of an o tensor, for n qubits"
	oten2paul[oten_]:=Module[{j,lst,nq,pten=oten,qq,tr=TensorRank[oten],
theta={ {1,0,0 ,1 },
        {0,1,1 ,0 },
	{0,I,-I,0 },
	{1,0,0 ,-1}  }  },
nq=tr/2; qq=tr-1;
		While[qq >=nq,
pten = Flatten[pten,1];
pten = theta . pten;
lst=Table[j-1,{j,qq}]; lst[[1]]=qq;
pten=transpose[pten,lst];
--qq;
		];
pten/2^nq] (*END oten2paul*)

		outer::usage = "outer[ls1, ls2, ...] gives the outer product"
	outer[args__]:= Outer[Times,args]

		partrace::usage = "M'=partrace[M,q,dl] traces M over space q
(=1 or 2 or ...)  in the list dl of factors in a tensor product.  Both M and M'
are square matrices."
	partrace[mat_,q_,dl_] := Module[{t=transpose[mat2oten[mat,dl],permptrace[Length[dl],q]]},
 oten2mat[  Sum[ t[[i,i]],{i,Length[t]} ]   ]]

		partrace2::usage = "partrace2[M,q] Traces 2^n matrix M over
qubit q (=1.2...)"
	partrace2[m_,q_]:= Module[{t=transpose[mat2oten2[m],permptrace[Log[2,Length[m]],q]]},
		oten2mat[t[[1,1]]+t[[2,2]]]];

		partrans::usage = "partrans[mt,q,dl] performs a partial
transpose on the matrix mt with respect to space q (=1, 2, etc.) on a tensor
product of spaces corresponding to dimension list dl. E.g., q=2, dl={2,3},
transposes on the 3 dimensional space."
	partrans[mt_,q_,dl_]:= oten2mat[transpose[mat2oten[mt,dl],permtrans[Length[dl],q]]];

		partrans2::usage = "partrans2[mt,q] returns partial transpose
of mt over qubit q in a tensor product of qubits"
	partrans2[mt_,q_]:= oten2mat[transpose[mat2oten2[mt],permtrans[Log[2,Length[mt]],q]]];


		paul2mat::usage = "paul2mat[ptn] takes a tensor of
coefficients in the sum ptn[[i,j,...]] sig[i] x sig[j] x ... and returns the
corresponding matrix"
	paul2mat[ptn_]:=oten2mat[paul2oten[ptn]];

		paul2oten::usage = "paul2oten[ptn] takes a tensor ptn of
Pauli coefficients and generates the corresopnding o-form (dyad) tensor.
Inverse of oten2paul"
	paul2oten[ptn_]:=Module[{j,lst,nq=TensorRank[ptn],otn=ptn,
thetab={{1,0,0 ,1 },
        {0,1,-I,0 },
	{0,1, I,0 },
	{1,0,0 ,-1}  }  },
lst=Table[j-1,{j,nq}]; lst[[1]]=nq;
	For[j=0,j<nq,++j,
otn = thetab . otn;
otn=transpose[otn,lst];
	];
otn=Flatten[otn];
While[Length[otn] > 2, otn=Partition[otn,2] ];
otn] (*END paul2oten*)



		paulnz::usage="paulnz[ptn] forms a list
{{label1,entry1},{label2,entry2},...} of nonzero elements of the Pauli tensor
ptn, where label is a string of the form {i, j, k...}  and the entry is
ptn[[i+1,j+1,...]]. E. g. {1,0,3} labels the coef. of (sig_x ox I ox sig_z)."
	paulnz[ptn_]:=Module[{digs,jp,lp=TensorRank[ptn],
np=4^TensorRank[ptn],outlist={},plist=Flatten[ptn]},
	For[jp=1,jp<=np,++jp,
If[ 0== plist[[jp]], Continue[]];
digs=IntegerDigits[jp-1,4,lp];
AppendTo[outlist,{digs,plist[[jp]]} ];
	]; outlist] (*END paulnz*)

		paulnzch::usage="paulnzch[ptn,ep] forms a list
{{label1,entry1},{label2,entry2},...} of elements of the Pauli tensor ptn which
are nonzero in the sense that Chop[...,ep] is not 0. Here label is a string of
the form {i, j, k...}  and the entry is ptn[[i+1,j+1,...]]. E. g. {1,0,3}
labels the coef. of (sig_x ox I ox sig_z)."
	paulnzch[args__]:=Module[
  {digs,ep=10^-10,jp,largs=List[args],lp,np,outlist={},plist},
If[ 1 < Length[largs], ep = largs[[2]] ];
lp=TensorRank[ptn]; np=4^lp;
plist=Chop[ Flatten[ptn],ep ];
	For[jp=1,jp<=np,++jp,
If[ 0== plist[[jp]], Continue[]];
digs=IntegerDigits[jp-1,4,lp];
AppendTo[outlist,{digs,plist[[jp]]} ];
	]; outlist] (*END paulnzch*)

		paulten::usage="paulten[1,0,3] will generate the Pauli tensor
corresponding to sg_x ox I ox sg_z, and similarly for any number of arguments,
each of which must be an integer between 0 and 3."
	paulten[args__] := Module[{ls = List[args],ln,lsf,lsp,ptn},
ln=Length[ls]; lsp=1+ls; lsf=Table[4,{ln}];
ptn=Array[0*#&,lsf]; ptn = ReplacePart[ptn,1,lsp]; ptn] (*END paulten*)


		permket::usage = "permket[kt,pm,dl] returns ket corresponding
to kt on tensor product with dimension list dl when order of factors is
permuted according to pm. E.g., kt defined on A ox B ox C, dl = {4,2,3},
pm={2,3,1},the new ket is defined on the 3 x 4 x 2 space C ox A ox B";
	permket[kt_, pm_, dl_] := kten2ket[transpose[ket2kten[kt, dl], pm]];

		permket2::usage = "permket2[kt,pm] returns ket for a tensor
product of qubits in the permuted order corresponding to pm.  E.g. for kt
defined on A ox B ox C, pm={2,3,1}, the new ket is defined on C ox A ox B."
	permket2[kt_,pm_]:= Flatten[ transpose[ket2kten2[kt],pm] ];

		permmat::usage = "permmat[mt,pm,dl] Converts matrix mt
according to permutation pm of tensor product with dimension list dl"
	permmat[mt_,pm_,dl_]:=    oten2mat[
      nten2oten[
        transpose[ oten2nten[mat2oten[mt,dl]] ,  Join[pm,pm+Length[pm]] ]]];

		permmat2::usage = "permmat[mt,pm] Converts matrix mt to form
corresponding to permutation pm of tensor product of qubits"
	permmat2[mt_,pm_]:=  oten2mat[
    nten2oten[
      transpose[ oten2nten[mat2oten2[mt]] , Join[pm,pm+Length[pm]] ]]];

		permptrace::usage = "permptrace[n,q] returns a permutation of
the integers 1,2,3, ... 2n such that 2q-1 and 2q are moved to the beginning of
the list.  Thus for n=3 and q = 2 it returns 3,4,1,2,5,6.  Used to form a
partial trace"
permptrace[n_,q_]:=Array[If[#<(2*q-1),#+2,If[#>(2*q),#,If[OddQ[#],1,2]]]&,2*n];

		permno::usage = "permno[n] is permutation list taking
(1a,2a,3a,...na,1b,2b,...,nb) to (1a,1b,2a,2b,...,na,nb), namely
(1,3,5,...,2n-1,2,4,...,2n)."
	permno[n_]:= Join[ -1+2*Table[i,{i,n}],2*Table[i,{i,n}] ]

		permon::usage = "permon[n] is permutation list taking 
(1a,1b,2a,2b,...na,nb) to (1a,2a,3a,...na,1b,2b,...nb); namely
{1,n+1,2,n+2,3,n+3...}."
	permon[n_]:=Flatten[ Table[{i,i},{i,n}] + Table[{0,n},{n}] ]

		permtrans::usage = "returns the permutation 
	(1a,1b,2a,2b,...,qb,qa,...)";
  permtrans[n_,q_]:=Array[If[#==2*q,2*q-1,If[#==(2*q-1),2*q,#]]&,2*n]

		permute::usage = "permute[ls,pm] returns permutation of list
ls determined by pm. E.g., ls={a,b,c}, pm={2,3,1} returns {c,a,b}."
	permute[list_,pm_]:=list[[ invperm[pm] ]]


		permutmat::usage = "permutmat[pm] returns a permutation matrix
corresponding to the permutation list pm.  E.g. pm={2,3,1} will yield a matrix
'permt' which applied to the ket {a,b,c} will yield {c,a,b}; also 
(permt.oper.adjoint[permt]) for an operator on qubit 3 will yield the
corresponding operator on qubit 1"
	permutmat[pm_] := Module[{len=Length[pm],w,j},
w=Table[0,{j,1,len},{k,1,len}];
For[j=1,j<=len,++j, w[[ pm[[j]],j ]] = 1;]; w]


		plabc::usage = "plabc[ls] takes, e.g., {2,0,3} and turns
it into a string like c[2,0,3]= ."
	(*plabc:comment. Helper function for prtpaul[], prtpaulch[]*)	
	plabc[ls_] := Module[{jj,ln=Length[ls],str="  c["},
str =  str<>ToString[ ls[[1]] ];
		For[jj=2,jj<=ln,++jj,
str = str<>","<>ToString[ ls[[jj]] ];
		];
str = str<>"]= "; str] (*END plabc*)


		pop2dop::usage= "pop2dop[mt] takes a positive operator
represented by the matrix mt and returns the corresponding density operator
matrix: mt divided by its trace."
	pop2dop[mt_]:= mt/Tr[mt];

		prodlist::usage = "prodlist[ls] returns the product of the
elements in list ls."
	prodlist[ls_] := Product[ls[[i]],{i,Length[ls]}]


		prtpaul::usage = "prtpaul[ptn] uses Mca Print[] to output
nonzero elements of the Puali tensor ptn in the form c[3,0,2]= ... for
the coefficient corresponding to sg_z ox I ox sg_y."
	(*prtpaul:comment. paulnz[] extracts nonzero elements, and plabc[]
produces a label in the form c[..].*)
	prtpaul[ptn_] := Module[  {list,llist,newfn},
newfn[x_] := ReplacePart[x,plabc[ x[[1]] ],1];
list=paulnz[ptn]; llist = Map[newfn,list];
Apply[ Print,Flatten[llist] ];  ](*END prtpaul*)

		prtpaulch::usage = "prtpaulch[ptn,ep] uses Mca Print[] to
output nonzero--in sense that Chop[,ep] is not 0--elements of the Puali tensor
ptn in the form c[3,0,2]= ... for the coefficient corresponding to sg_z ox I ox
sg_y. Single argument ptn results in default ep determined by paulnzch"
	(*prtpaulch:comment. paulnzch[] extracts nonzero elements, and plabc[]
produces a label in the form c[..].*)
	prtpaulch[args__] := Module[  {list,llist,newfn},
newfn[x_] := ReplacePart[x,plabc[ x[[1]] ],1];
list=paulnzch[args]; llist = Map[newfn,list];
Apply[ Print,Flatten[llist] ];  ](*END prtpaulch*)


		quadn::usage = "quadn[ml] is the sum of the absolute squares
of the elements in ml, whether a scalar, vector, matrix or tensor."
(*Comment: Use Re[] to get rid of 0.I terms in output*)
	quadn[m_]:= Module[{lng=Length[m]},
If[0==lng,Return[Conjugate[m]*m]];
Re[Conjugate[Flatten[m]].Flatten[m]] ];

		quadr::usage = "quadr[ml] is the sum of the squares
of the elements in ml, assumed to be a REAL vector or matrix or tensor."
	quadr[m_]:= Module[{lng=Length[m]},
If[0==lng,Return[m^2]]; Flatten[m].Flatten[m]]

		(*Random functions*)
	Needs["Statistics`NormalDistribution`"]
		ranbas::usage = "ranbas[n] returns a random basis of an
n-dimensional Hilbert space"
	ranbas[n_] := ranorn[n,n]

		ranbasr::usage = "ranbas[n] returns a random real basis of an
n-dimensional Hilbert space"
	ranbasr[n_] := ranornr[n,n]

(*


		ranbell::usage = "Returns a list of four 4d kets which 
form a random fully-entangled basis for two qubits."
	ranbell := Module[{jr,kets={},smat,vmat,wmat},
vmat = ranbas[2]; wmat = ranbas[2];
	For[jr=0,jr<4,++jr,
smat = vmat . sig[jr] . wmat;
AppendTo[kets, 
  (ketprod[{1,0}, smat. {1,0}] + ketprod[{0,1}, smat. {0,1}])/Sqrt[2.]];
	]; kets]

*)

(*END ranbell*)

		ranket::usage = "ranket[n] generates a normalized random ket
for an n-dimensional complex Hilbert space"
	ranket[n_]:= Module[{}, ketnorm[
RandomArray[NormalDistribution[0,1],n]+
I*RandomArray[NormalDistribution[0,1],n] ] ](*END ranket*)

		ranketr::usage = "ranketr[n] generates a normalized real random
ket for an n-dimensional Hilbert space"
	ranketr[n_]:= Module[{j,ket={}},
	For[j=1,j<=n,++j,
ket = Append[ ket, Random[NormalDistribution[0,1]] ];
	]; 
ketnorm[ket] ]

		ranorn::usage = "ranorn[m,n] randomly generates m orthonormal
kets on a space of dimension n, as a list of m lists of n terms"
	ranorn[m_,n_]:= Module[ { j,kets={} },
If[ m > n, Return["ranorn[m,n] called with m > n"] ];
	For[j=0,j<m,++j,
kets = Append[ kets,ranket[n] ];
	];
grschm[kets] ]

		ranornr::usage = "ranornr[m,n] randomly generates m orthonormal
real kets on a space of dimension n, as a list of m lists of n terms"
	ranornr[m_,n_]:= Module[ { j,kets={} },
If[ m > n, Return["ranornr[m,n] called with m > n"] ];
	For[j=0,j<m,++j,
kets = Append[ kets,ranketr[n] ];
	];
grschmr[kets] ]

		rgate::usage = "Single qubit rgate[j,th] for j=1,2,3=(x,y,z)
rotates by angle th about axis j as per Nielsen and Chuang p. 174"
	rgate[j_,th_]:= Cos[th/2] sig[0] - I Sin[th/2] sig[j];


		(*Special Bell basis and conversion functions*)
sbell[0] = {0,1,-1,0}/Sqrt[2];
sbell[1] = {I,0,0,-I}/Sqrt[2]; 
sbell[2] = {1,0,0,1}/Sqrt[2]; 
sbell[3] = {0,-I,-I,0}/Sqrt[2];

sbellbas=transpose[Table[sbell[j],{j,0,3}]]  (* sbellbas . sbellket = ket *)
bassbell=adjoint[sbellbas] (* bassbell . ket = ket in special Bell basis *)

		sbell2mat::usage = "sbell2mat[mat_] converts a 4 x 4 matrix in
the special Bell basis to one in the standard basis"
	sbell2mat[mat_]:= sbellbas . mat . bassbell

		schmidt::usage = "schmidt[ket,dl] takes a ket, assumed
normalized, on a tensor product AxB, with dl = {dim A, dim B}, expands it in
the Schmidt form as a sum of type c_j |a_j> |b_j>, with c_j > 0, and returns a
list {c_j, |a_j>, |b_j>}, where |a_j> and |b_j> are themselves lists. For |c_j|
< 10^-8, nothing is returned."
	(*schmidt:comment. The cutoff for |c_j| is Sqrt[eps]*) 
	schmidt[ket_,dl_]:= Module[
{basa,bkets,eps = 10^-16,j,list,rho,rhoa,snorm},
rho=dyad[ket,ket];
rhoa = partrace[rho,2,dl];
basa = grschm[Eigenvectors[rhoa]];
bkets = ketcofs[ket,basa,dl];
list={};
	For[j=1,j<=dl[[1]],++j,
snorm = quadn[ bkets[[j]] ];
If[ snorm < eps ,Continue[] ];
AppendTo[ list,{Sqrt[snorm],basa[[j]],ketnorm[ bkets[[j]] ]} ];
	];
list]
(*END schmidt*)


		schmidtprobs::usage = "schmidtprobs[ket,dl] returns Schmidt
probabilities for normalized 'ket' on tensor product A x B with dl={da,db}."
	schmidtprobs[ket_,dl_] :=Module[{rho,rhoa},
rho=dyad[ket,ket];
rhoa = partrace[rho,2,dl];
Chop[ Re[Eigenvalues[rhoa]] ]    ]
(*END schmidtprobs*)

		schmidtproj::usage = "Takes a list {c_j,|a_j>,|b_j>} and forms
the projector sum_j |a_j><a_j x |b_j><b_j|, ignoring the c_j."
	schmidtproj[ls_]:=Module[{i,l=Length[ls],ketp,proj},
ketp=ketprod[ ls[[1,2]],ls[[1,3]] ]; proj = dyad[ketp,ketp];
	For[i=2,i<=l,++i,
ketp = ketprod[ ls[[i,2]],ls[[i,3]] ];
proj += dyad[ketp,ketp];
	];
proj ]
(*END schmidproj*)


		schmidt2ket::usage = "Applied to a list {c_j, |a_j>, |b_j>},
where |a_j> and |b_j> are themselves lists, returns the sum_j c_j*|a_j>x|b_j>."
	schmidt2ket[ls_]:= Sum[ ls[[i,1]]*ketprod[ ls[[i,2]],ls[[i,3]] ],
{i,Length[ls]} ];

(*

		sevencode::usage = "{ |0_L>, |1_L> } for Steane 7 qubit code"
	sevencode = {
 +bket[0000000,7] +bket[1010101,7] +bket[0110011,7] +bket[1100110,7]
 +bket[0001111,7] +bket[1011010,7] +bket[0111100,7] +bket[1101001,7],
 +bket[1111111,7] +bket[0101010,7] +bket[1001100,7] +bket[0011001,7]
 +bket[1110000,7] +bket[0100101,7] +bket[1000011,7] +bket[0010110,7] }/Sqrt[8]

*)


		(* Pauli sigma matrices *)
sig[0] = {{1,0},{0,1}};
sig[1] = {{0,1},{1,0}};
sig[2] = {{0,-I},{I,0}};
sig[3] = {{1,0},{0,-1}};

		sigl::usage = "sigl[ls] returns a tensor product of Pauli
matrices corresponding to the list ls.  E.g., ls={0,2} produces sig[0] otimes
sig[2] as a matrix."
   sigl[l_] := sigl[l] = Fold[tenprod, sig[First[l]], Map[sig[#] &, Rest[l]]];

		sigprod::usage = "sigprod[j,k,...] = sig[j] ox sig[j] ox...
as a matrix; j, k, ... integers in [0,3].  Any number of arguments."
	sigprod[args__]:=sigl[List[args]]

(* small bug fixed by GJL in following function *)

		sumlist::usage = "sumlist[ls] returns the sum of the elements
in list ls."
	sumlist[ls_] := Sum[ ls[[i]],{i,Length[ls]} ]

		tenprod::usage = "tenprod[mt1,mt2,...] returns the matrix of
the tensor product mt1 0x mt2 0x ... The matrices may be rectangular."
(*tenprod:comment. outer[] transforms the na matrices read in
to an o-tensor, which is transformed to an n-tensor by transpose[] using
the permutation pm. The product of numbers of columns of matrices
read in = dim =  number of columns of output matrix.*)
	tenprod[args__]:=Module[{dim,las=List[args],na,pm},
na=Length[las]; pm = permon[na]; 
dim=prodlist[ Map[Last[Dimensions[#]]&,las] ];
Partition[  Flatten[ transpose[outer[args],pm] ] ,  dim  ]](*END tenprod*)

(*

		threecode::usage = "list { |000>, |111>}"
	threecode = {bket[000,3], bket[111,3]}

*)

		traceout::usage = "traceout[mt,ls,dl] takes the partial trace
of mt over the spaces in list ls which are among those forming the tensor
product corresponding to the list dl.  E.g., let dl={2,3,4}, ls = {1,3}; the 2
and 4 dimensional spaces are traced out to leave a 3x3 matrix."
	traceout[mt_,ls_,dl_] := If[ Length[ls]==0 , mt , 
traceout[
	partrace[mt,First[Reverse[Sort[ls]]],dl],
	Rest[Reverse[Sort[ls]]],
	Drop[dl,{First[Reverse[Sort[ls]]]}]
	]			   ](*END traceout*)

		traceout2::usage = "traceout2[mt,ls] Partial trace of matrix mt
over all qubits in list ls"
	traceout2[mt_,ls_] := Fold[partrace2[#1, #2] &, mt, Reverse[Sort[ls]]];

		transpose::usage = "transpose[ket] returns ket, not an error
message; otherwise, transpose[]=Transpose[]"
	transpose[args__] := Module[{},
If[TensorRank[List[args][[1]]] < 2, Return[List[args][[1]]],
Return[Transpose[args]],Print["Error in transpose"] ] ];

		xgate::usage = "X (sigma_x) or NOT gate on 1 qubit"
	xgate=sig[1]

		xket::usage = "xket[0], xket[1] are +x and -x axis states
on Bloch sphere"
	xket[0]={1,1}/Sqrt[2]
	xket[1]={1,-1}/Sqrt[2]

		xprj::usage = "xprj[0], xprj[1] project on +x and -x axis
states on Bloch sphere"
	xprj[0]=dyap[ xket[0] ]
	xprj[1]=dyap[ xket[1] ]

		ygate::usage = "Y (sigma_y) gate on 1 qubit"
	ygate=sig[2]

		yket::usage = "yket[0], yket[1] are +y and -y axis states
on Bloch sphere"
	yket[0]={1,I}/Sqrt[2]
	yket[1]={1,-I}/Sqrt[2]

		yprj::usage = "yprj[0], yprj[1] project on +x and -x axis
states on Bloch sphere"
	yprj[0]=dyap[ yket[0] ]
	yprj[1]=dyap[ yket[1] ]

		zgate::usage = "Z (sigma_z) gate on 1 qubit"
	zgate=sig[3]

		zket::usage = "zket[0]=|0>, zket[1]=|1> are +z and -z axis
states on Bloch sphere"
	zket[0]={1,0}
	zket[1]={0,1}

		zprj::usage = "zprj[0]=|0><0|, zprj[1]=|1><1| project on +x and
-x axis states on Bloch sphere"
	zprj[0]=dyap[ zket[0] ]
	zprj[1]=dyap[ zket[1] ]

(* needed to add blank line so Fateman parser would succeed *)

