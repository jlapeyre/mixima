Array[f, 10]

Array[1 + #^2 &, 10]

Array[f, {3, 2}]

Array[10 #1 + #2 &, {3, 4}]

Array[f, 10, 0]

Array[f, {2, 3}, {0, 4}]

(* following example not in docs *)
Array[f, {2, 3}, 4]

Array[FromDigits[{##}] &, {2, 3, 4}]

Array[a, {2, 3}, 1, Plus]

Array[0 &, {3, 3}]

Array[Signature[{##}] &, {3, 3, 3}]

Boole[Array[Greater, {5, 5}]]

m = Array[Subscript[a, ##] &, {3, 4}]

Array[Power, {5, 5}]

Array[Array[x &, #] &, {6}]

