## Translation of Qinf from Mathematica to Mixima

### Qinf

Quantum information program by Robert Griffiths. See [./LICENSE](LICENSE)

The original distribution with documentation is available at
http://quantum.phys.cmu.edu/QPM/index.html.
The documentation is not included here.

### Usage

To use the Macsyma translation
1) Install the compatibility functions as described in the distribution above.
2) Start maxima and load the code

```
(%i1) load("mixima");
(%o1)                 /home/user/.maxima/mixima.mac
(%i2) load("./qinf050.mac");
(%o2)                            ./qinf050.mac
```

To test some of the routines with application code, start maxima and give these commands.
```
load("mixima");
batch("./rtest_qinf.mac",test);
```

An application of the Quantum Information Programs is in `misc10.ma`. The translation is
in `rtest_misc10.mac` formatted as a Maxima test. To run the test, enter Maxima and type
```
load("mixima");
batch("./rtest_misc10.mac",test);
```

Note for some of the above you may have to do `load("./qinf050.mac")` twice in a row.
Performance may be improved by compiling the code and/or the Mixima functions.

### Files

Most of these files are intermediate files from the translation. They are useful for studying
the translation, but are not necessary for using the quantum information package.

* qinf050.ma -- the original Mma source code to the software

* qinf050_mod.m -- a few hand edits to qinf050.ma
             to allow the parser to read it, and to fix a 
             few small errors.

*  diff_qinf050_modqinf -- the difference between qinf050.ma and qinf050_mod.m

* qinf050_mod_raw.mac -- raw output of translator mtrans on qinf050_mod.m

* qinf050.mac -- raw output reformatted by function in reformat.lisp

* qinf_byhand.mac -- file to be loaded afer qinf050.mac that fixes some
                  translation errors.

* rtest_qinf.mac -- tests of qinf.mac (not complete!)

* misc10.ma -- Some application code that uses qinf050.ma

* misc10_raw.mac -- translation by mtrans

* misc10_mod.mac -- hand edits to trim garbage, mistranlsations etc.

* misc10_reformatted.mac -- reformatted by stringout(filename,input)

* rtest_misc10.mac  -- test of translation of misc10

