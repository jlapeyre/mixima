(clrhash *mix-help-hash*)
(help-item-init "ArrayDepth")

(help-item-add-example "ArrayDepth"
  (list 
  :type "ok"
  :maxin "ArrayDepth([[a,b],[c,d]]);"
  :maxout "2;"
 ))

(help-item-add-example "ArrayDepth"
  (list 
  :type "ok"
  :maxin "ArrayDepth([[a,b],[c]]);"
  :maxout "1;"
 ))

(help-item-add-example "ArrayDepth"
  (list 
  :type "ok"
  :maxin "ArrayDepth(f(f(a,b),f(c,d)));"
  :maxout "2;"
 ))

(help-item-add-example "ArrayDepth"
  (list 
  :type "ok"
  :maxin "Dimensions(Array(a,[4,5,2]));"
  :maxout "[4, 5, 2];"
 ))

(help-item-init "Array")

(help-item-add-example "Array"
  (list 
  :type "ok"
  :maxin "Array(f,10);"
  :maxout "[f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9), f(10)];"
 ))

(help-item-add-example "Array"
  (list 
  :type "ok"
  :maxin "Array(lambda( [arg1], (1+(arg1^2)) ),10);"
  :maxout "[2, 5, 10, 17, 26, 37, 50, 65, 82, 101];"
 ))

(help-item-add-example "Array"
  (list 
  :type "ok"
  :maxin "Array(f,[3,2]);"
  :maxout "[[f(1, 1), f(1, 2)], [f(2, 1), f(2, 2)], [f(3, 1), f(3, 2)]];"
 ))

(help-item-add-example "Array"
  (list 
  :type "ok"
  :maxin "Array(lambda( [arg1,arg2], ((10*arg1)+arg2) ),[3,4]);"
  :maxout "[[11, 12, 13, 14], [21, 22, 23, 24], [31, 32, 33, 34]];"
 ))

(help-item-add-example "Array"
  (list 
  :type "ok"
  :maxin "Array(f,10,0);"
  :maxout "[f(0), f(1), f(2), f(3), f(4), f(5), f(6), f(7), f(8), f(9)];"
 ))

(help-item-add-example "Array"
  (list 
  :type "ok"
  :maxin "Array(f,[2,3],[0,4]);"
  :maxout "[[f(0, 4), f(0, 5), f(0, 6)], [f(1, 4), f(1, 5), f(1, 6)]];"
 ))

(help-item-add-example "Array"
  (list 
  :type "ok"
  :maxin "Array(f,[2,3],4);"
  :maxout "[[f(4, 4), f(4, 5), f(4, 6)], [f(5, 4), f(5, 5), f(5, 6)]];"
 ))

(help-item-add-example "Array"
  (list 
  :type "ok"
  :maxin "Array(\"^\",[5,5]);"
  :maxout "[[1,1,1,1,1],[2,4,8,16,32],[3,9,27,81,243],[4,16,64,256,1024],
[5,25,125,625,3125]];"
 ))

(help-item-add-example "Array"
  (list 
  :type "br"
  :maxin "/* Array(lambda( [], 0 ),[3,3]);"
  :maxout ""
 ))

(help-item-add-example "Array"
  (list 
  :type "br"
  :maxin "Array(lambda( [], Signature([SLOTSEQUENCE(1)]) ),[3,3,3]);"
  :maxout ""
 ))

(help-item-add-example "Array"
  (list 
  :type "br"
  :maxin "Boole(Array(MGREATERP,[5,5]));"
  :maxout ""
 ))

(help-item-add-example "Array"
  (list 
  :type "br"
  :maxin "(m:Array(lambda( [], Subscript(a,SLOTSEQUENCE(1)) ),[3,4]));"
  :maxout ""
 ))

(help-item-init "Coefficient")

(help-item-add-example "Coefficient"
  (list 
  :type "ok"
  :maxin "Coefficient(((x+1)^3),x,2);"
  :maxout "3;"
 ))

(help-item-add-example "Coefficient"
  (list 
  :type "ok"
  :maxin "Coefficient(((x+y)^4),(x*(y^3)));"
  :maxout "4;"
 ))

(help-item-add-example "Coefficient"
  (list 
  :type "ok"
  :maxin "Coefficient(((a*x)+(b*y)+c),x);"
  :maxout "a;"
 ))

(help-item-add-example "Coefficient"
  (list 
  :type "ok"
  :maxin "Coefficient(((a*(x^3))+(b*(x^2))+(c*x)+d),x,2);"
  :maxout "b;"
 ))

(help-item-add-example "Coefficient"
  (list 
  :type "ok"
  :maxin "Coefficient((((x+2)^2)+((x+3)^3)),x,0);"
  :maxout "31;"
 ))

(help-item-add-example "Coefficient"
  (list 
  :type "ok"
  :maxin "Coefficient(((x+y)*(x+(2*y))*((3*x)+(4*y)+5)),(x*(y^2)));"
  :maxout "18;"
 ))

(help-item-init "AtomQ")

(help-item-add-example "AtomQ"
  (list 
  :type "ok"
  :maxin "block([e : x + 22/7],
AtomQ(e));"
  :maxout "false;"
 ))

(help-item-add-example "AtomQ"
  (list 
  :type "ok"
  :maxin "AtomQ(22/7);"
  :maxout "true;"
 ))

(help-item-add-example "AtomQ"
  (list 
  :type "ok"
  :maxin "AtomQ(\"this is an atom\");"
  :maxout "true;"
 ))

(help-item-add-example "AtomQ"
  (list 
  :type "ok"
  :maxin "AtomQ(ThisIsAnAtom);"
  :maxout "true;"
 ))

(help-item-add-example "AtomQ"
  (list 
  :type "ok"
  :maxin "AtomQ(1.23);"
  :maxout "true;"
 ))

(help-item-add-example "AtomQ"
  (list 
  :type "ok"
  :maxin "AtomQ(1 + 2*%i);"
  :maxout "true;"
 ))

(help-item-add-example "AtomQ"
  (list 
  :type "ok"
  :maxin "AtomQ(x);"
  :maxout "true;"
 ))

(help-item-add-example "AtomQ"
  (list 
  :type "ok"
  :maxin "AtomQ(\"string\");"
  :maxout "true;"
 ))

(help-item-add-example "AtomQ"
  (list 
  :type "ok"
  :maxin "AtomQ(123456);"
  :maxout "true;"
 ))

(help-item-add-example "AtomQ"
  (list 
  :type "ok"
  :maxin "AtomQ(1/10);"
  :maxout "true;"
 ))

(help-item-init "Apply")

(help-item-add-example "Apply"
  (list 
  :type "ok"
  :maxin "Apply(f,[a,b,c,d]);"
  :maxout "f(a, b, c, d);"
 ))

(help-item-add-example "Apply"
  (list 
  :type "ok"
  :maxin "Apply(Plus,[a,b,c,d]);"
  :maxout "d + c + b + a;"
 ))

(help-item-add-example "Apply"
  (list 
  :type "ok"
  :maxin "Apply(Plus,g[x,y,z]);"
  :maxout "z+y+x;"
 ))

(help-item-add-example "Apply"
  (list 
  :type "ok"
  :maxin "Apply(f,[[a, b], [c], d]);"
  :maxout "f([a,b],[c],d);"
 ))

(help-item-add-example "Apply"
  (list 
  :type "ni"
  :maxin ""
  :maxout ""
  :mma "Apply[f, {{a, b, c}, {d, e}}, {1}]"
 ))

(help-item-add-example "Apply"
  (list 
  :type "ni"
  :maxin ""
  :maxout ""
  :mma "Apply[f, {{{{{a}}}}}, -1]"
 ))

(help-item-add-example "Apply"
  (list 
  :type "ni"
  :maxin ""
  :maxout ""
 ))

(help-item-init "Chop")

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :maxin "Chop(exp(N(Range(4)* %pi  * %i)));"
  :maxout "[-1.0,1.0,-1.0,1.0];"
 ))

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :comments "symbol remains unsimplified"
  :maxin "Chop(a);"
  :maxout "a;"
 ))

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :maxin "Chop( 1 + 1e-20* %i - 7*(a + 1e-30* b)*%i);"
  :maxout "1-7*%i*a;"
 ))

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :comments "number smaller than chop_epsilon : 1e-10 in abs val  is set to zero"
  :maxin "Chop(1e-17);"
  :maxout "0;"
 ))

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :maxin "Chop(-1);"
  :maxout "-1;"
 ))

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :maxin "Chop(-1e-17);"
  :maxout "0;"
 ))

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :comments "maps over a list"
  :maxin "Chop([1,1e-20]);"
  :maxout "[1,0];"
 ))

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :comments "at mulitple levels"
  :maxin "Chop([1,1e-20, [1.4, 1e-19] ]);"
  :maxout "[1,0,[1.4,0]];"
 ))

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :maxin "Chop([1e-6,1e-17,1e-8]);"
  :maxout "[9.9999999999999995E-7,0,1.0E-8];"
 ))

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :maxin "Chop([1e-6,1e-17,1e-8],1e-3);"
  :maxout "[0,0,0];"
 ))

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :maxin "Chop(1+%i);"
  :maxout "%i+1;"
 ))

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :maxin "Chop(1+ 1e-20 * %i);"
  :maxout "1;"
 ))

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :maxin "Chop(1e-20 + 3 * %i);"
  :maxout "3*%i;"
 ))

(help-item-add-example "Chop"
  (list 
  :type "ok"
  :maxin "Chop(Chop(1e-6 + 3 * %i) - (3*%i + 1e-6));"
  :maxout "0;"
 ))

(help-item-init "Table")

(help-item-add-example "Table"
  (list 
  :type "ok"
  :comments "note this does not return a list"
  :maxin "Table(1);"
  :maxout "1;"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "Table(i^2, [i, 10]);"
  :maxout "[1,4,9,16,25,36,49,64,81,100];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :comments "test that variable in surrounding scope does not interfere with iteration var.
 not an mma example."
  :maxin "block([i], i:1, Table(i,[i,1,3]));"
  :maxout "[1,2,3];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "Table(f(i), [i, 0, 20, 2]);"
  :maxout "[f(0),f(2),f(4),f(6),f(8),f(10),f(12),f(14),f(16),f(18),f(20)];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "Table(x,[10]);"
  :maxout "[x,x,x,x,x,x,x,x,x,x];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "Table(10*i + j, [i, 4], [j, 3]);"
  :maxout "[[11,12,13],[21,22,23],[31,32,33],[41,42,43]];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "Table(f(i), [i, 10, -5, -2]);"
  :maxout "[f(10),f(8),f(6),f(4),f(2),f(0),f(-2),f(-4)];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "Table(f[i], [i, 10, -5, -2]);"
  :maxout "[f[10],f[8],f[6],f[4],f[2],f[0],f[-2],f[-4]];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :comments "limits of j depend on i."
  :maxin "Table(10* i + j, [i, 5], [j, i]);"
  :maxout "[[11],[21,22],[31,32,33],[41,42,43,44],[51,52,53,54,55]];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "Table(100* i + 10* j + k, [i, 3], [j, 2], [k, 4]);"
  :maxout "[[[111,112,113,114],[121,122,123,124]],
[[211,212,213,214],[221,222,223,224]],
[[311,312,313,314],[321,322,323,324]]];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :comments "supply explicit list"
  :maxin "Table(sqrt(x), [x, [1, 4, 9, 16]]);"
  :maxout "[1,2,3,4];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "Table(j^(1/i), [i, [1, 2, 4]], [j, [1, 4, 9]]);"
  :maxout "[[1,4,9],[1,2,3],[1,sqrt(2),sqrt(3)]];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "Table(2^x + x, [x, a, a + 5* n, n]);"
  :maxout "[2^a+a,2^(n+a)+n+a,2^(2*n+a)+2*n+a,2^(3*n+a)+3*n+a,2^(4*n+a)+4*n+a,
2^(5*n+a)+5*n+a];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "apply( lambda([x,y], Table(i - j, [i, x], [j, y])), [4,5]);"
  :maxout "[[0,-1,-2,-3,-4],[1,0,-1,-2,-3],[2,1,0,-1,-2],[3,2,1,0,-1]];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "Table( block(print(i), i^i^i), [i, 3]);"
  :maxout "[1,16,7625597484987];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "Table(Last(IntegerDigits(x, 2)), [x, [1,6,1,0,0,7,9,8]]);"
  :maxout "[1,0,1,0,0,1,1,0];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "map(lambda([x],Last(IntegerDigits(x, 2))), [1,6,1,0,0,7,9,8]);"
  :maxout "[1,0,1,0,0,1,1,0];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :comments "following two examples are equivalent"
  :maxin "Table(i + j, [i, 3], [j, i]);"
  :maxout "[[2],[3,4],[4,5,6]];"
 ))

(help-item-add-example "Table"
  (list 
  :type "ok"
  :maxin "Table(Table(i + j, [j, i]), [i, 3]);"
  :maxout "[[2],[3,4],[4,5,6]];"
 ))

(help-item-init "Append")

(help-item-add-example "Append"
  (list 
  :type "ok"
  :maxin "Append([a, b, c, d], x);"
  :maxout "[a,b,c,d,x];"
 ))

(help-item-add-example "Append"
  (list 
  :type "ok"
  :maxin "Append(f[a, b, c], x + y);"
  :maxout "f[a,b,c,y+x];"
 ))

(help-item-add-example "Append"
  (list 
  :type "ok"
  :maxin "map(lambda([y], Append(y, x)) , [[a, b], [c, d]]);"
  :maxout "[[a,b,x],[c,d,x]];"
 ))

(help-item-add-example "Append"
  (list 
  :type "br"
  :maxin "Map(lambda([y], Append(y, x)) , [[a, b], [c, d]]);"
  :maxout ""
 ))

(help-item-add-example "Append"
  (list 
  :type "br"
  :maxin "NestList(lambda([y],Append(y,x)), [a], 5);"
  :maxout ""
 ))

(help-item-init "Complement")

(help-item-add-example "Complement"
  (list 
  :type "ok"
  :maxin "Complement([a,b,c,d,e],[a,c],[d]);"
  :maxout "[b,e];"
 ))

(help-item-init "Characters")

(help-item-add-example "Characters"
  (list 
  :type "ok"
  :maxin "Characters(\"A string.\");"
  :maxout "[\"A\",\" \",\"s\",\"t\",\"r\",\"i\",\"n\",\"g\",\".\"];"
 ))

(help-item-add-example "Characters"
  (list 
  :type "ok"
  :maxin "StringJoin(Characters(\"A string.\"));"
  :maxout "\"A string.\";"
 ))

(help-item-add-example "Characters"
  (list 
  :type "ni"
  :maxin "Characters([\"ABC\", \"DEF\", \"XYZ\"]);"
  :maxout ""
 ))

(help-item-add-example "Characters"
  (list 
  :type "ok"
  :maxin "Partition(Characters(\"this is a string\"), 3, 1);"
  :maxout "[[\"t\",\"h\",\"i\"],[\"h\",\"i\",\"s\"],[\"i\",\"s\",\" \"],[\"s\",\" \",\"i\"],
[\" \",\"i\",\"s\"],[\"i\",\"s\",\" \"],[\"s\",\" \",\"a\"],[\" \",\"a\",\" \"],
[\"a\",\" \",\"s\"],[\" \",\"s\",\"t\"],[\"s\",\"t\",\"r\"],[\"t\",\"r\",\"i\"],
[\"r\",\"i\",\"n\"],[\"i\",\"n\",\"g\"]];"
 ))

(help-item-add-example "Characters"
  (list 
  :type "ok"
  :maxin "StringJoin(Partition(Characters(\"this is a string\"), 3, 1));"
  :maxout "\"thihisis s i isis s a a a s ststrtririning\";"
 ))

(help-item-add-example "Characters"
  (list 
  :type "ok"
  :maxin "Sort(Characters(\"this is a string\"));"
  :maxout "[\" \",\" \",\" \",\"a\",\"g\",\"h\",\"i\",\"i\",\"i\",\"n\",\"r\",\"s\",\"s\",\"s\",\"t\",\"t\"];"
 ))

(help-item-add-example "Characters"
  (list 
  :type "br"
  :comments "Mma returns this unevaluated. This implementation returns errors"
  :maxin "Characters(x + y);"
  :maxout ""
 ))

(help-item-init "AppendTo")

(help-item-add-example "AppendTo"
  (list 
  :type "ok"
  :maxin "block( [ ls : [a, b, c, d] ],
AppendTo(ls, x),
ls);"
  :maxout "[a,b,c,d,x];"
 ))

