#!/usr/bin/perl -w

# Not really a call tree, but sort of like that.
#
# This parses capsonlyparser.lisp or other version of the parser and
# print a partial call tree. This is very crude. It assumes we are in
# a new function whenever we see 'defun'. Discards a certain list of
# functions and of calls. 
# Make two passes. First collect funcs by looking for 'defun'
# Then pass again, looking for appearances of these func names  and
# assume they were called by the most recent 'defun' function.


@lines = ();

while(<>) {
    chomp;
    push @lines,$_;
}

foreach (@lines) {
    if (/defun\s+([^\s\(\)]+)/) {
	$fun = $1;
	$flag = 0;
	foreach $ex ( qw(  p ps pc rc guess-token peek-token var-p   )) {
	    $flag = 1 if $ex eq $fun;
	}
	next if $flag == 1;
	$funs{$1}++;
    }
}

@funcs = sort keys %funs;



foreach (@lines) {
    if (/defun\s+([^\s\(\)]+)/) {
	$curfun = $1;
	$calls{$curfun} = {};
#	next; dont do next, because a call can occur on same line as defun
    }
    foreach $f (@funcs) { # check if line is call to a func
	if ( /\($f\s+/ ){  # note that calls all have '(' first. and in fact no space before
                           # identifier, then a space
	    $calls{$curfun}->{$f} ++ if defined $curfun;
	}
    }
}

foreach(@funcs) {
    print $_,"\n";
    foreach $f ( sort keys %{$calls{$_}}) {
	print "    $f\n";
    }
}
