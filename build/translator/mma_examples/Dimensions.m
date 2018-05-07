Dimensions[{{a, b, c}, {d, e, f}}]

Dimensions[{{a, b, c}, {d, e}, {f}}]

Dimensions[{{{{a, b}}}}]

Dimensions[{{{{a, b}}}}, 2]

Dimensions[f[f[x, y], f[a, b], f[s, t]]]

Dimensions[f[g[x, y], g[a, b], g[s, t]]]

(* this won't work, because the array does not work this way for us 
  but constantarray will do it *)
Dimensions[Array[a, {2, 1, 4, 3}]]
