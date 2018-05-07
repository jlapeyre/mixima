(* qinf/programs/misc10.ma 5/3/10
=graph01.ma 5/25/07

	Taken from graph01.ma
	Graph states for small graphs (2,3,4,5 qubits).
	See code04.ma for Vaidman states

		Notes: 
 IQNTG-5j for graph states
 IQNTE-75e for Vaidman 4 qubit states (&> code04.ma)


	The procedure used in the programs is as follows.  A state of the form
ketplusn = |++...+> is created for n qubits.  Then a set of controlled-phase
gates are applied, assuming the qubits are numbered 1 to n.  The product of
these (they commute) is taken and applied to ketplusn.  This produces a graph
state. In some cases, in order to see what the resulting state is in a +-
representation, a gate haprodn, which is a product of Hadamard on all the
qubits, is applied and then the result converted to a binary 0 or 1
representation, where in fact 0 stands for + and 1 for -.

	0. Definitions
	2. 2 qubit states
	3. 3 qubit states
	4. 4 qubit states
	5. 5 qubit states

*)

<<qinf050.ma
in:= In[1]
Print["misc10.ma"]

			(*0. Definitions *)

absq[x_]:= ComplexExpand[Conjugate[x]*x] (*Absolute square*)
had=Sqrt[2]*hgate (*Unnormalized Hadamard*)
pket = {1,1} (* |+> *)
idn = sig[0] (* I for qubit *)

			(*0. END Definitions *)

			(*2. 2 qubit states*)
Print["Part 2:"]
ketplus2 = ketprod[pket,pket]
grstate = cphase.ketplus2
grrho = dyadr[grstate,grstate]
grpaul = mat2paul[grrho]
prtpaul[grpaul]

			(*2. END 2 qubit states*)


			(*3. 3 qubit states*)
Print["Part 3:"]
ketplus3 = ketprod[pket,pket,pket]
haprod3 = tenprod[had,had,had]

cphase12 = expandout2[cphase,{1,2},3]
cphase23 = expandout2[cphase,{2,3},3]
cphase31 = expandout2[cphase,{3,1},3]

		(*Linear*
lingr3 = cphase12.cphase23.ketplus3
grrho = dyadr[lingr3,lingr3]
grpaul = mat2paul[grrho]
prtpaul[grpaul]
		*END Linear*)

		(*Cyclic*)
cyccps = cphase31.cphase23.cphase12
cycgr3 = cyccps.ketplus3
cycgr3b = ket2bin[ cyccps.ketplus3 ]
cycgrp3 = ket2bin[ haprod3.cyccps.ketplus3 ]
grrho = dyadr[cycgr3,cycgr3]
grpaul = mat2paul[grrho]
prtpaul[grpaul]


			(*3. END 3 qubit states*)

			(*4. 4 qubit states*)
Print["Part 4:"]

haprod3 = tenprod[had,had,had]
ketplus4 = ketprod[pket,pket,pket,pket]
haprod4 = tenprod[had,had,had,had]

cphase12 = expandout2[cphase,{1,2},4]
cphase13 = expandout2[cphase,{1,3},4]
cphase14 = expandout2[cphase,{4,1},4]
cphase23 = expandout2[cphase,{2,3},4]
cphase24 = expandout2[cphase,{4,2},4]
cphase34 = expandout2[cphase,{3,4},4]



		(*Cyclic or square*)
cyccps = cphase14.cphase34.cphase23.cphase12
cycgr4 = ket2bin[ cyccps.ketplus4 ]
cycgrp4 = ket2bin[ haprod4.cyccps.ketplus4 ]

		(*Linear*)
lincps = cphase34.cphase23.cphase12
lingr4 = ket2bin[ lincps.ketplus4 ]
lingrp4 = ket2bin[ haprod4.lincps.ketplus4 ]

		(*Star*)
strcps = cphase12.cphase13.cphase14
strgr4 = strcps.ketplus4 
strgr4b = ket2bin[ strcps.ketplus4 ]
strgrp4 = ket2bin[ haprod4.strcps.ketplus4 ]

		(*Triangle with arm*)
tracps=  cphase12.cphase13.cphase14.cphase34
tragr4 = ket2bin[ tracps.ketplus4 ]
tragrpp4 = ket2bin[ tenprod[sig[0],haprod3]. tracps.ketplus4 ]

		(*Crossed square*)
csqcps = cphase12.cphase13.cphase14.cphase23.cphase24
csqgrpp4 = ket2bin[ tenprod[idn,idn,had,had]. csqcps.ketplus4 ]

		(*Tetrahedron*)
tetcps = cphase12.cphase13.cphase14.cphase23.cphase24.cphase34
tetgr4 = ket2bin[ tetcps.ketplus4 ]
tetgrp4 = ket2bin[ haprod4.tetcps.ketplus4 ]

		(*Pauli representation*)
gket = strgr4
grrho = dyadr[gket,gket]
grpaul = mat2paul[grrho]
prtpaul[grpaul]
		(*END Pauli representation*)
			(*4. END 4 qubit states*)


			(*5. 5 qubit states*)
Print["Part 5:"]

ketplus5 = ketprod[pket,pket,pket,pket,pket]
haprod5 = tenprod[had,had,had,had,had]

cphase12 = expandout2[cphase,{1,2},5]
cphase23 = expandout2[cphase,{2,3},5]
cphase34 = expandout2[cphase,{3,4},5]
cphase45 = expandout2[cphase,{4,5},5]
cphase51 = expandout2[cphase,{5,1},5]

		(*Cyclic*)
cyccps = cphase51.cphase45.cphase34.cphase23.cphase12
cycgrp5 = ket2bin[ haprod5.cyccps.ketplus5 ]

			(*5. END 5 qubit states*)


