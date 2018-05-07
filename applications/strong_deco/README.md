# Strongdeco

### Efficiency

By default this is rather slow, as is seen by  `Laug(5);`.
Compiling and then loading `grobner` makes it much faster.
```
compile_file(file_search("grobner.lisp"),"./grob.o");
load("./grob.o");
```

One may also try `lex` or `grlex` here:
 `poly_monomial_order: lex;`.

I advise not using `Permanent` defined in the strongdeco code. Rather use the Mixima builtin,
which calls the maxima builtin. It is much faster. Comment out or otherwise
don't load the definition of Permanent in strongdeco.


### Files

The following is a sequence of files and comments on what was done to go from the Mathematica text to working
Mixima code for the main function.

* Strongdeco-tobias.txt -- exported text from Mma nb. Not in a very useful form.

* strong_deco.m   --  copied functions mostly by hand into Mma 3.0 and did
  ? funcname and copied formatted  functions  back into this file. Added
   comment markers around free text.

* strong_deco_nocomments.m -- should be about the same as above, but no comments.
   Note in the two previous files, some things were fixed by hand because they gave
   errors, Mma should have complained if it did not:
   The colon was missing here: `zzz[a_, b_] := a^b`.
   Also in this file, you may need  a semi-colon after each definition. The parser
   may die otherwise. But, this may be completely fixed by now.


* sd_raw.mac -- Give the shell command  `mmatomax strong_deco_nocomments.m > sd_raw.mac`
  In maxima batch-process the previous file  `batch("./sd_raw.mac")$`
  Then print reformatted code to a file:
  `newstringout("./sd_reform.mac", [1,10000],2);`.
   Finally, edit out the lines giving the batch command, etc.

* sd_reform_fix.mac  copy of `sd_raw.mac` with by-hand fixes. Include fixes
     for `Pots` that are discussed elsewhere. add `simpproduct:true` to start of file.
     (In the mean time, we added/modded some functions in the compatibility functions
     in order to get things to work.
     Loading this file and running `rootscontract(LaugDeco(3))` or 4 or 5 gives correct results.
      But some of the other functions do not yet work.

<!--  LocalWords:  Strongdeco Laug grobner lex grlex monomial Mixima
 -->
<!--  LocalWords:  strongdeco mma tobias txt nb deco funcname zzz sd
 -->
<!--  LocalWords:  nocomments mmatomax newstringout simpproduct
 -->
<!--  LocalWords:  LaugDeco rootscontract
 -->
