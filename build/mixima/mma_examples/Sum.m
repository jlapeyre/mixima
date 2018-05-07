(* This is a subset of the Sum examples from the documentation. *)
Sum[i^2, {i, 10}]

Sum[i^2, {i, 1, n}];

Sum[1/i^6, {i, 1, Infinity}]

Sum[i^3, i]

Sum[1/(j^2 (i + 1)^2), {i, 1, Infinity}, {j, 1, i}]

Sum[f[i], {i, 1, 4}]

Sum[f[i], {i, 1, 4, 2}]

Sum[f[i], {i, {a, b, c}}]

Sum[f[i, j], {i, 1, 3}, {j, 1, 3}]

Sum[f[i, j], {i, 1, 3, 2}, {j, 1, 3, 1/2}]

Sum[f[i, j], {i, 1, 3}, {j, 1, i}]

Sum[f[i, j], {i, {a, b}}, {j, 1, 2}]

list = Table[
   Graphics[{c, Disk[]}, ImageSize -> 20], {c, {Red, Green}}];
Sum[f[i, j], {i, list}, {j, 1, 2}]

Sum[1/i^2, {i, Infinity}]

Sum[1/(i^2 + 1), {i, -Infinity, Infinity}]

Sum[1/(1 + i + j)^3, {i, Infinity}, {j, Infinity}]

Sum[i, {i, n}]

Sum[1/(1 + i + j), {i, n}, {j, m}]

Sum[i j, i, j]

Sum[i j, {i, 0, n}, i]

Sum[i^3 + (i + 3)^5, {i, 1, n}]

Sum[(i + j) 3^(i - j), {i, 1, n}, {j, 1, m}]

Sum[i^2 a^i, {i, 0, n}]

Sum[i^2 a^i, {i, Infinity}]

Sum[i^2 Fibonacci[i]/2^i, {i, Infinity}]

Sum[1/(i (i + 6)), {i, n}]

Sum[1/(i^2 + 1), {i, Infinity}]

Sum[1/(i (2 i + 1)), {i, Infinity}]

Sum[(2^i (-1 + i))/(i (1 + i)), {i, n}]

Sum[2^(-i)/(i (i + 1)), {i, Infinity}]

Sum[Sin[i + b] Cos[i], {i, 1, n}]

Sum[i Sin[i], {i, n}]




