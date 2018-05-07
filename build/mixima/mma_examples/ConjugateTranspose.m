m = {{1, 2 I, 3}, {3 + 4 I, 5, I}}
ConjugateTranspose[m]
Block[{j = 0}, t = Table[(++j) + I (++j), {2}, {3}, {4}]]
ConjugateTranspose[t]
ConjugateTranspose[t, {3, 2, 1}]
