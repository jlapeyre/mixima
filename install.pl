#!/usr/bin/perl -w
use strict;
use File::Copy;
our @Bin_files;   # from file_lists.pl
our %Inst_files;  
our @Files_needing_mixima_install_path;
our $Lisp_interpreter_for_scripts;

our $Install_share_dir;
our $Install_bin_dir;
our $Mixima_share_dir;
our $Build_dir;


#-----------------------------------------------------------------

# This file tries to install the mixima code in
# Maxima's default system directory on a POSIX system, or
# some other directory that you specify.  If this does not
# work correctly, you need to copy the files by hand to the
# appropriate directory/folder. Moving all the files from
# ./compatibility and ./translator and ./tests to a folder
# should be enough.

#-----------------------------------------------------------------

require "./file_lists.pl";
require "./config_install.pl";

if (not -e $Build_dir) {
    die("Can't find '$Build_dir'. You must first build the distribution with 'mkdistcompat.pl'")
}

check_install_directories();
install_share_files();
install_bin_files();

#-----------------------------------------------------------------
sub check_install_directories {
    make_mixima_install_dir_name();
    print("mkdir( $Mixima_share_dir,0755)\n");
    if (not -e $Mixima_share_dir) 
        { mkdir($Mixima_share_dir,0755) or die "Failed to create directory '$Mixima_share_dir'";}
    if (not -d $Mixima_share_dir) {
        die "Failed to create directory $Mixima_share_dir";
    }
}

sub install_share_files {
    die "Installation directory '$Mixima_share_dir' does not exist" unless -d $Mixima_share_dir;
    foreach my $name (keys %Inst_files ) {
        my $files = $Inst_files{$name};
        print "------ Installing $name files\n";
        my ($srcdir,$junk) = get_src_targ_dirs($name);
        $srcdir = $Build_dir . '/' . $srcdir;
        foreach (@$files) {
            print "copy($srcdir/$_, $Mixima_share_dir/$_)\n";
            copy("$srcdir/$_", "$Mixima_share_dir/$_") or die 
                "copy($srcdir/$_, $Mixima_share_dir/$_) failed";
            print "chmod(0644,$Mixima_share_dir/$_)\n";
            chmod(0644,"$Mixima_share_dir/$_") or die
                "chmod(0644,$Mixima_share_dir/$_) failed";
        }
    }
}

sub install_bin_files {
    my @bins = @Bin_files;
    my $targ = $Install_bin_dir;
    if ( not -e $targ and not -d $targ) {
        die "Can't find binary installation directory '$targ'";
    }
    die unless defined $Build_dir and -e $Build_dir;
    my $srcdir = $Build_dir . '/bin';
    foreach my $n (@bins) {
        $n = $n->{name}  if  ref($n) eq 'HASH';
        if ( $n =~ s/^(gcl|sbcl)\.// ) { # gcl or sbcl scripts have been chosen
            next unless $1 eq $Lisp_interpreter_for_scripts;
        } 
	print "copy($srcdir/$n, $targ/$n)\n";
	copy("$srcdir/$n", "$targ/$n") or die
	    "copy($srcdir/$n, $targ/$n) failed";
	print "chmod(755,$targ/$n)\n";
	chmod(0755,"$targ/$n") or die
	    "chmod(755,$targ/$n) failed";
    }
}
