#!/usr/bin/perl

$trans_bin = "./mtrans";

@files = (`ls ./mma_examples/*.m`,
	  `ls ./our_mma_tests/*.m`);


sub dosys { print STDERR $_[0],"\n"; system $_[0];}
foreach(@files) { chomp};

foreach my $ifile (@files) {
    my $ofile = $ifile;
    $ofile =~ s/\.m$/.ex.mac/;
    if ($ifile eq $ofile) {die "ifile eq ofile"}
    dosys( "$trans_bin $ifile > $ofile");
}
