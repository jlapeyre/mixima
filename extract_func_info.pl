#!/usr/bin/perl -w

require "./file_lists.pl";
our @Compatibility_install_files;



#  extract_func_info.pl
#  This script extracts some documentation from the source code
#  of the mixima functions.
#  Comments identifying functions are searched for. In a .mac file, we look for
#  /*> Function Somefunction */
#  In a .lisp file, we look for
#  #|> Function Somefunction |#



$basedir = "./compatibility";


# @files = qw(
#   mixima_funcs.mac  mixima_matrix.lisp  mixima_table.lisp mixima_simplify.lisp
#   mixima_list.lisp  mixima_sum_product.lisp mixima_derivatives_integrals.lisp
#   mixima_roots.lisp mixima_limit.lisp mixima_newton.mac mixima_polynomials.mac
#   mixima_tests.lisp  mixdoc.lisp
#     );

@files = @Compatibility_install_files ;


# These files are already in the above list.

@shadow_mathfiles = (
#  'mixima_shadow_math_functions.mac'
    );

@shadow_files = (
#  'mixima_shadow.mac'
    );


our (%funcs, %macfuncs, %shadow_math, %shadow, %allfuncs);

foreach $file ( @files ) {
    extract_from_file($file,\%funcs, $basedir);
}

foreach $file ( @shadow_mathfiles ) {
    extract_from_file($file,\%shadow_math, $basedir);
}

foreach $file ( @shadow_files ) {
    extract_from_file($file,\%shadow, $basedir);
}

foreach $file ( qw( mockmma-mixima-shell.lisp  ) ) {
    extract_from_file($file,\%shadow, './translator');
}

# $file -- input, file to read from
# $funcs -- input, ref to hash to hold functions names
sub extract_from_file {
   my($file,$funcs,$dir) = @_;
    $funcs = {} unless defined $funcs;
    print STDERR " extract_func_info.pl processing '$file'\n";
    if ( $file eq 'function_list.lisp' or $file eq 'helpitems.lisp'
     or $file eq 'mixdoc.lisp') {
        print STDERR "**** Skipping $file\n";
        return;
    }
    open IH, "<$dir/$file" or die "*** Can't open $dir/$file for reading";
    my ($key,$name,@rest);
    while(<IH>) {
#	if ( /^(\/\*>|\#\|>)\s+(\w+)\s+(\w+)\s+(\w*)/){
	if ( s/^(\/\*>|\#\|>)// ) {
            s/\*\/\s*//;
            s/\|\#\s*//;
            ($key,$name,@rest) = split;
            if ( @rest > 0 and defined $rest[0] and $rest[0]) {
		$macfuncs->{$name} = $rest[0];
            }
	    if  ($key eq 'Function') {
		$funcs->{$name}++;
		$allfuncs{$name}++; # list of all of them
		if ($allfuncs{$name}>1) {print STDERR "DUPLICATE! $name\n";}
	    }
	    if  ($key eq 'LispFunction') { # not used
		$funcs->{$name}++;
	    }
	    if  ($key eq 'AuxFunction') {
		$auxfuncs{$name}++;
	    }
	    if  ($key eq 'Notes') {
		$notes{$name} = '';
		while( my $note = <IH>) {
		    last if $note =~ /\*\//;
		    $notes{$name} .= " " .$note;
		}
		$notes{$name} =~ s/^\s+(.+)/$1/;
		chomp($notes{$name});
	    }
	}
    }
    close IH;
}

sub print_funcs {
    my ($funcs,$stream) = @_;
    foreach $key ( sort keys %$funcs ) {
	print $stream "$key";
	print $stream "  -->  " . $macfuncs->{$key} if exists $macfuncs->{$key};
	print $stream " -- " .$notes{$key} if exists $notes{$key};
	print $stream "\n";
    }
    print $stream "\n";
}

# count number of keys in hash
sub nfuncs {
    my ($href) = @_;
    my $n = keys %$href;
    return $n;
}

sub print_function_list {
    my $stream;
    open $stream, ">./function_list" or die "*** Can't open function_list";
    
print $stream <<ENDTXT;
Mixima:  Mathematica compatibility functions for Maxima VERSION VERSIONNUMBER

Below is a list of compatibility functions for Maxima that
behave like Mathematica functions.  For some functions, the
Mma behavior is only partially implemented.  For many
functions, the test file rtest_mixima.mac (and others)
contain examples of which features are implemented and notes
1on which are not.

Some functions in Maxima and Mma take arguements in exactly
the same way and return the same results. These are noted
below with a '-->' giving the name of the equivalent Maxima
functions. This equivalence often holds only for the basic
usage, while extended features of the functions
differ. There is no advantage in using the Mma name for
functions that are marked with '-->'.  Ideally, these
functions should be rewritten to reproduce the extended Mma
behavior. But here, they are essentially aliases. These
'shadow' functions are provided for convenience, and will
work in most or many circumstances, but in general, it is
safer to use the native Maxima function. For instance Cos(x)
calls cos(x), but there are some instances where it is
unevaluated and then the simplifier will not recognize Cos
as cos for trigonometric identities. In any case, there is
no advantage in using Cos for cos.

But some other shadowed functions have very different names
and are unlikely to cause simplifier problems (ie Riffle is
equivalent to join ).

Some functions that you might expect are shadowed, are not shadowed.
For instance 'atom' and 'AtomQ' have slightly different behavior.

These functions are all loaded with load("mixima") by default, but are
in separate files so that, for instance, trig and special functions need
not be loaded.

ENDTXT

#$totalfuncs = nfuncs(\%funcs) +  nfuncs(\%shadow) + nfuncs(\%shadow_math);

#print "$totalfuncs compatibility functions.\n\n";

print $stream nfuncs(\%allfuncs) . "  Mathematica compatibility functions\n\n";
print_funcs(\%allfuncs, $stream);

print $stream "Helper Functions:\n";
foreach $key ( sort keys %auxfuncs ) {
    print $stream "$key\n";
}
print $stream  "\n";
    close($stream);
    
}
    


sub print_online_list {
    my $stream;
    open $stream, ">./compatibility/function_list.lisp" or die "*** Can't open function_list.lisp";
    print $stream "(setf mixima-function-list (list \n";
    foreach $key ( sort keys %allfuncs ) {
        print $stream '"' . $key . '"' . "\n";
    }
    print $stream "))\n";
    close($stream);
}    

print_function_list();
print_online_list();

=pod

print "\nThe following repeats the list above, but with the shadowed functions listed separately\n\n";

print nfuncs(\%funcs) . " Mathematica functions (not shadowed)\n";
print_funcs(\%funcs);

print "\n" . nfuncs(\%shadow) . " Non-math (more or less) shadowed functions\n";
print_funcs(\%shadow);

print "\n" . nfuncs(\%shadow_math) . " Shadowed math functions\n";
print_funcs(\%shadow_math);

=cut



