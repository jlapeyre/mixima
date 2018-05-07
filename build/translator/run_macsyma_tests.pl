#!/usr/bin/perl

$trans_bin = "./mtrans";

@files = (`ls ./mma_examples/*.m`,
	  `ls ./our_mma_tests/*.m`);


sub dosys { print STDERR $_[0],"\n"; system $_[0];}
foreach(@files) { chomp};

dosys("rm -f ./trans_errs");
foreach my $ifile (@files) {
    my $ofile = $ifile;
    my $exfile = $ifile;
    $ofile =~ s/\.m$/.test.mac/;
    $exfile =~ s/\.m$/.ex.mac/;
    if ($ifile eq $ofile) {die "ifile eq ofile"}
    dosys( "$trans_bin $ifile > $ofile");
    dosys("diff $ofile $exfile");
    system("diff $ofile $exfile >> ./trans_errs");
    dosys("rm -f $ofile");
}

print "ERRORS:\n";
system("cat ./trans_errs");
