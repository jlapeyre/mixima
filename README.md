# Mixima

## Mathematica compatibility functions and translator for the Maxima computer algebra system.

This software is independent from *Wolfram* and is neither
supported by, nor endorsed by *Wolfram Research*.

*Mathematica* is a registered trademark of *Wolfram Research, Inc.*

### Help

If you have any difficulty, please [file an issue](https://github.com/jlapeyre/mixima/issues).

## Introduction

This software has four parts:

1. Compatibility functions for [Maxima](http://maxima.sourceforge.net/) that try to duplicate
 Mathematica (Mma) functions, eg `Table`. Used alone,
 they provide a quasi-Mma environment. Mixima is designed to
 extend the maxima environment more or less like any other
 package-- not to replace it, as does Mockmma.  The
 compatibility functions are Maxima functions called with
 Maxima syntax from within Maxima.

2.  A translator that translates a subset of Mma code to
 Maxima code.  The compatitiblity functions can be used by
 themselves or with this translator. But the translator
 produces code that depends on the compatibility functions.

3. A mock Mma mode that allows using Mathematica syntax at
  the command line. This is sort of a Mma clone. The parser is
  a modified version of the Richard Fateman's Mockmma project. The allowed
  syntax and semantics is limited  to that available when
  the Mockmma parser was written.

4. A version of Robert Griffiths' [Quantum Information Programs in Mathematica](http://quantum.phys.cmu.edu/QPM/)
   translated to Maxima is included here. This was the original motivation for Mixima.
   It is installed together with the mixima compatibility functions.

There are more READMEs at the top level and some information
can be found in [doc/mixima_user_doc](doc/mixima_user_doc). See also the section on
online help at the bottom of this document.

## Motivation and context

1. I wrote a quantum-information package, [qinf](https://github.com/jlapeyre/qinf), for Maxima to
support reproducible research. It was able to verify computations done by hand in a particular
already-published paper.

2. I found that progress could be made by translating existing
quantum-information packages for Mathematica (Wolfram) to Maxima. This
translation, in turn, is much easier if one writes compatibility functions
for the basic functions called by the Mathematica package.  A package by
Robert Griffiths was a great candidate because it a) Supports the
calculations done in the papers I am interested in, and b) Uses a
restricted subset of the Mathematica language. In particular, it does not
use advanced pattern matching. This package, `Mixima`, is the result of this
process. It successfully translates and runs Griffiths' quantum-information code,
as well as an existing package for studying many-body physics. These are both found
in the [applications folder](./applications/).

3. The translated application code [strong_deco](./applications/strong_deco/) runs correctly but
is algorithmically inefficient, which becomes important when the length of the objects studied
is large enough. This is due to a fundamental difference in the way expressions are represented
in the two languages. So, I implemented an alternate, efficient, representation for expressions in Maxima and
supporting code. This code and other third-party packages are organized in the
[mext repository](https://github.com/jlapeyre/mext). The compatibiliy functions and tooling in `mext`
are far more developed than the code here. But, in mext, there is more deviation from the Mathematica API,
and there is no translator.

4. In the meantime a language, [Julia](https://julialang.org/), has arrived that is suitable for symbolic computation.
Its ecosystem is far more developed than those of `Maxima` and `Common Lisp`, and is growing.
This allows much greater productivity. So I wrote [Symata](https://github.com/jlapeyre/Symata.jl) in
Julia. Symata is similar in many ways to Mathematica and Maple.


## Install

### MS Windows and other OSs

If you have maxima installed, this software
should work in principle. It is tested a bit on Win32; the testsuite
passes. For Windows, copy the entire folder .\build\mixima
to your maxima share folder.  For instance, copy the
folder to
 `c:\Program Files\Maxima-5.25.1-gcl\share\maxima\5.25.1\share`
Then you should have a folder
`c:\Program Files\Maxima-5.25.1-gcl\share\maxima\5.25.1\share\mixima`
with all the mixima files in it.

### Linux/unix system

Edit the file (config_install.pl) to choose where the
files are installed. By default, the source is installed
in `~/.maxima/mixima/` and the executables in `~/bin/`.
Then type the shell command `./mkdistcompat.pl`.
Then type the shell command `./install.pl`.

If  this does not work, try copying (./build/mixima) to a
location where your maxima installation can see it.

## Testing

If mixima is installed, give the maxima commands
```
load("mixima/mixima");
mixtest();
```

Or in this toplevel directory, start maxima and give the following commands:
`load("./tests/mixima_testsuite.mac");`  or  `load("./tests/mixima_testsuite_slow.mac");`.

### Known problems

* The mockmma shell does not currently work well with `xmaxima` nor
 `wxmaxima`. The compatibility functions seem to work ok.

* `wxmaxima` has a bug that will print an error message involving STRIPDOLLAR.
  Mixima can  still be used, but the state of maxima is changed and and some things
 are broken. To avoid this, remove the line that loads `loadlast.mac` in
 the file `mixima.mac`. The offending function is `Clear()`.

## Quick start

The compatibility functions are functions written in the
Maxima language that emulate the behavior of Mathematica
(Mma) functions. For example:

* Compatibility Functions
```
> maxima
(%i1) load("mixima/mixima");
(%o1)          /usr/share/maxima/5.21.1/share/mixima/mixima.mac
(%i2) Table( i*j, [i,3],[j,3]);
(%o2)                  [[1, 2, 3], [2, 4, 6], [3, 6, 9]]
(%i3) mixFuncs();       /* lists compatibililty functions */
(%i4) mixDoc(Table);    /* prints examples */
```

*  entering the mockmma shell to enter  Mma code at the command line

```
(%i5) tomma();
In[6]:= Array[#1^#2 &,{3,4}]

(%i6) Array(lambda( [[lambda_args]], (lambda_args[1]^lambda_args[2]) ),[3,4])
(%o6) [[1, 1, 1, 1], [2, 4, 8, 16], [3, 9, 27, 81]]

Out[6]:= {{1, 1, 1, 1}, {2, 4, 8, 16}, {3, 9, 27, 81}}
In[7]:= Exit
(%o7)                                true
(%i8) %o6;
(%o8)            [[1, 1, 1, 1], [2, 4, 8, 16], [3, 9, 27, 81]]
```

`tomma()` does not work with xmaxima and wxmaxima.

* Evaluating strings of Mma code.
```
(%i1) smmatomax(" a = 1 ");
(%o1)                                  1
(%i2) a;
(%o2)                                  1
```

`smmatomax()` should work with xmaxima and wxmaxima.

* translating code at the unix shell:  `mmatomax mma_code.m`


## Details

Mixima can be used several ways:

### Functions that extend Maxima with functions similar to those in Mma

```
 > maxima
(%i1) load("mixima/mixima");
(%o1)          /usr/share/maxima/5.21.1/share/mixima/mixima.mac
(%i2) Table( i*j, [i,3],[j,3]);
(%o2)                  [[1, 2, 3], [2, 4, 6], [3, 6, 9]]
```

Note that the syntax used here is Maxima syntax, not Mma. For instance

    lists: {} --> []
    function call: [] --> ()
    assignment:   = --> :  ,etc.

A list of the compatibility functions is included in the file
`function_list`. Example code can be found in the (./tests) folder
and in the (./applications) folder. The code in the tests folder contains comments
for some Mma functions concerning how much of those  functions are implemented.

As a convenience, the executable `mixima` loads the mixima code automatically
but does not currently allow other initialization scripts):

```
> mixima
...
(%i1) Tuples([0,1],2);
(%o1)                  [[0, 0], [0, 1], [1, 0], [1, 1]]
```

### Mixima can use the Mockmma shell.

For example

```
(%i1) tomma();
 ...
In[1]:= Table[ i j,{i,3},{j,3}]

Out[1]:= {{1, 2, 3}, {2, 4, 6}, {3, 6, 9}}

In[2]:= NIntegrate[1/Sqrt[x],{x,0,1},PrecisionGoal->12]

Out[2]:= 1.999999999999998

In[3]:= Exit
```

As a convenience, the executable `miximamma` starts Maxima and
the Mockmma shell with the Mixima translator.

Here are commands that can be used to control whether the results are
shown in  Maxima or Mma or both.

* `MmaShowOut[True]` (or False) to control Mma output
* `MmaShowTrans[True]` (or False) to control maxima display of translation.
* `MmaVerbose[True]` (or False) to print more stuff during evaluation.
* `MmaPrintEx[arg]`  print strings suitable for inserting in rtest files
      as tests and examples. If no arg is given , print no function name
      in the example. If a string is given, it is used as the function name
      for subsequent input. If arg is False, then disable.

### The original Mockmma is included

Mockmma is modified slightly from the upstream Mockmma distribution.
Usually, when we refer to mockmma, we don't mean this original,
but rather the mockmma shell modified to use the compatibility functions.

```
(%i1) :lisp (mockmma)
 ...
In[1]:=
```

As a convenience, the executable `mockmma` starts maxima and
Mockmma (code by Fateman et. al. modified by H.Monroe)

Note that for many inputs, Mockmma and Mixima with the Mockmma shell
give identical output. But in fact there are important difference that
are not explained here.

###  Using translator from maxima, Using stand alone translation scripts

For these, see (README.translator)

## Compatibility Functions

Due in part to fundamental differences int the two CAS's,
these compatibility functions reproduce the behavior of the
original functions imperfectly and to varying
degrees. Examples of ways in which they may be useful are

1. They can be used in Maxima projects by people who are already
    familiar with the Mma functions.
2. They can be used to more easily translate user Mma code to Maxima.
3. They may provide functionality not currently available in Maxima.

### Use of Compatibility functions

Load the functions from the Maxima prompt with
`(%i1) load("mixima/mixima");`.

The other source files will then be loaded automatically.
The files must have been placed in your Maxima search path
for this to work.

Examples of how to use the functions and some notes on limits in functionality
are in the test file `rtest_mixima.mac`.


### Matrices and Sets

* Maxima has a special data type for matrices, while Mma implements matrices
as ordinary lists. This is handled in the compatibility functions by allowing
either a Maxima list or a Maxima matrix as an argument that is to be interpreted
as a matrix. If the function returns a matrix, then, if possible, the output type
is the same as the input type.  For instace:
```
(%i29) Inverse( matrix( [1,2],[3,4]));
(%o29) matrix([-2,1],[3/2,-1/2])
(%i30) Inverse( [ [1,2],[3,4] ]);
(%o30) [[-2,1],[3/2,-1/2]]
```

* Likewise Mma implements sets as ordinary lists. These are handled by the
compatibility functions in the same way that matrices are. For instance:

```
 (%i34) Union( {1,2}, {3,4});
 (%o34) {1,2,3,4}
 (%i35) Union( [1,2], [3,4]);
 (%o35) [1,2,3,4]
```

* If the arguments are a both sets and lists, then the return type is the
same as the first argument.

* Use the compatibility function `Dot` and not the Maxima
`.` if you want the the Mma function `Dot`, which can also
be written in Mma in the `.` form. In other words do not use
the Maxima `.`  function for the Mma `.`, but rather use
the compatibility `Dot`. The function `Dot` does not accept Maxima
matrices (but they can be converted) because the two systems
are not compatible. Read the documentation on matrix multiplication
for both systems to see why.

* Useful functions are
   *  `ensuremat(m)`  Converts `m` to a maxima matrix if it is
     a list. Returns `m` unchanged if it is already a matrix.
   *  `ensurelist(m)` makes matrix m to a list if not already a list.

### Online Help

* `mixFuncs();`  lists compatibility functions and operators.

* `mixFuncs(string);`  lists compatibility functions and operators whose
 names contain string.

* `mixDoc(function);`
lists examples of a function, where function is a symbol or
a string. ie.  `mixDoc("Integrate");` or  `mixima_doc(Integrate);`.

* `mixDoc(Integrate,bad);` list unimplemented or failed examples.

Not all functions have examples.

###  Syntax

* `@@` can be used for `Apply`

* `->`  can be used for `Rule`.

### Mma shell

* Example of entering and using the shell.

Note below that the mma shell and maxima shell use the same
line numbers. Use `In[3]`, `Out[3]` to acess `%i3`, etc. The Mma parser
can't read `%i3` properly.
Note that in the mixima-mockmma shell, the maxima input and output is
printed.

```
(%i1) a : 1;
(%o1)                                  1
(%i2) tomma();

In[3]:= a
 (%i3) a
 (%o3) 1

Out[3]:= 1

In[4]:= b = 2
 (%i4) (b:2)
 (%o4) 2

Out[4]:= 2

In[5]:= In[1]
 (%i5) In(1)
 (%o5) a : 1

Out[5]:= MSETQ[a, 1]

In[6]:= Out[1]
 (%i6) Out(1)
 (%o6) 1

Out[6]:= 1

In[7]:= Exit
 (%i7) Exit
 (%o7) Exit
(%o7)                                true
(%i8) %o4;
(%o8)                                  2
(%i9)
```

* What happens when you use the shell ? You enter a line of
Mma code. It is parsed as Mma code. This is translated to
maxima code and evaluated by maxima. The resulting maxima
expression is then translated back to Mma and printed.

You can turn off the printing of the intermediate results.

```
In[10]:= MmaShowTrans[False]
(%i10) MmaShowTrans(False)
```

Unsetting maxima translation
```
Out[10]:= False

In[11]:= N[Pi,12]

Out[11]:= 3.1415926535897931

In[12]:= InString[11]

Out[12]:= N(Pi,12)
```

In this case, using `InString[11]` gives the desired
result. `In[11]` ( or `%i11` in Maxima) give errors because
they stored results passed throught the simplifier,
which are returned in a new context.

You can read Mma expressions from a file with
`miximaEvalFile("filename");`.

Or from withing the mockmma shell
`miximaEvalFile["./translator/t1.m"]`.

* Commands related to the mockmma shell

These "commands" are functions that can be usefully
called from withing the maxima shell as well; eg
`In(3)` rather than `In[3]`.

  *  `InString[n]`
The nth (eg 3rd) mma input string from user's input,
returned by the parser, with whitespace and commas
stripped. This is stored as %is3.  Recall that the line
numbers and input and output are the same in the two shells.
Thus, if the nth line was entered in the maxima shell, there
will be no corresponding mma string and `InString[n]` returns
an atom, eg %is3.


  * `mInString[n]`
The nth (eg 3rd) maxima input string built by translating the
mma input string. Also available as %ims3, but this is
not valid syntax in the mockmma shell.

  *  `In[n]`
The nth maxima input expression. Currently only printed
in "2d" form. This expression is created by the parser from
the user's mma input string. This is the correct expression
to use in the mockmma shell. This is the same as %i3, etc.
in the maxima shell. The expressions entered in the mockmma
shell are copied to `%i{n}` so that they are available
within the maxima shell; also via `In(n)`.

  * `Out[n]`

The nth maxima output expression. This is the correct
way to access the output for further calculation
from within the mockmma shell.

## Acknowledgments

A lot of invaluable advice and code snippets were provided by Maxima developers. In particular, Robert
Dodier, Richard J Fateman, Stavros Macrakis, and Barton Willis.
