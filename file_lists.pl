# lists of files to be included in the scripts that make and
# install the package.
# NOTE: The version number of the distribution is set here.

use strict;

#                       Set  Version number Here
#----------------------------------------------------------------

            our               $VERSION   =   '0.25'  ;

#----------------------------------------------------------------

our $Packname = "mixima";
#our $Releasedir = "../releases";
our $Dist_top_dir = "./build";
our $Build_dir =  "./build";
our $Webdir = "$Dist_top_dir/web";
our $Tarname = "$Dist_top_dir.tar.gz";
our $Zipname = "$Dist_top_dir.zip";
our $Install_share_dir;
our $Install_bin_dir;
our $Mixima_share_dir;
require "./config_install.pl";


our %Main_dirs = (
    Compatibility => "./compatibility",
    Translator => "./translator",
    Tests => "./tests",
    Application => "./applications",
    Doc => "./doc",
    Bin => "./bin",
    Top => "."
);

our %Dist_main_dirs;

# make the names of the distribution directories.
foreach my $dirname (keys %Main_dirs ) {
    $Dist_main_dirs{$dirname} = $Dist_top_dir . '/' . $Main_dirs{$dirname};
}


# These will be installed to the maxima share directory
our @Compatibility_mac_install_files =
    qw (
           dttodiff.mac
           general.mac
           loadlast.mac
           list.mac
           vars.mac
           flow_control.mac
           devel.mac
           mma_translator.mac
           mixima.mac
           newton.mac
           polynomials.mac
           combinatorics.mac
           matrix.mac
           predicates.mac
           shadow.mac
           shadow_math_functions.mac
   );


# These will be installed to the maxima share directory
our @Compatibility_lisp_install_files = qw (
 function_list.lisp
 helpitems.lisp
 mixdoc.lisp
 derivatives_integrals.lisp
 limit.lisp
 list.lisp
 flow_control_lisp.lisp
 expressions.lisp
 matrix.lisp
 roots.lisp
 simplify.lisp
 sum_product.lisp
 table.lisp
 predicates.lisp
 incr_decr.lisp
 block.lisp
 assign.lisp
 newblock.lisp
 reformat.lisp
 helper.lisp
 defs.lisp
 function.lisp
);

# These go in the distribution, but are not installed
our @Compatibility_lisp_files_for_build = qw (
 mixdoc.in.lisp
);

# Test suite files
our @Test_files = qw (
 testsuite.mac
 testsuite2.mac
 testsuite3.mac
 testsuite_slow.mac
 rtest_mixima.mac
 rtest_assign.mac
 rtest_combinatorics.mac
 rtest_function.mac
 rtest_expressions.mac
 rtest_flow_control.mac
 rtest_table.mac
 rtest_findroot.mac
 rtest_nintegrate.mac
 rtest_primes.mac
 rtest_list.mac
 rtest_slow.mac
 rtest_special_functions.mac
 rtest_matrix.mac
 rtest_sets.mac
 rtest_incr_decr.mac
 rtest_transpose.mac
 rtest_translator1.mac
 rtest_translator2.mac
 rtest_qinf050.mac
 rtest_strongdeco.mac
 
 );

##############################################################################
our @Translator_source_files_requiring_versions =
    qw(
      mma-to-mixima.lisp
      mockmma-mixima-shell.lisp
  );


our @someTranslator_source_install_files =
    qw (
           bf.lisp
           builtinsyms.lisp
           disp1.lisp
           eval.lisp
           grindfile.lisp
           jmma.lisp
           match.lisp
           maxima.lisp
           mixima-mockmma-parser.lisp
           mma-to-mixima.lisp
           mma.lisp
           mockmma-mixima-shell.lisp
           modgriffiths.mac
           parser_patch.lisp
           pf.lisp
           reformat.lisp
           stack1.lisp
           uconsalt.lisp
   );

our @Translator_source_install_files = (@someTranslator_source_install_files,@Translator_source_files_requiring_versions);
    
# other files in the translator directory that are perhaps useful
our @Translator_other_files = qw (
    NOTES-translator
    gcl.mmatomax
    gcl.mmatomax_int
    sbcl.mmatomax
    sbcl.mmatomax_int
    call_tree.pl
    run_macsyma_tests.pl
    create_macsyma_tests.pl
);

our @Files_needing_mixima_install_path = qw (
     ./translator/gcl.mmatomax
     ./translator/gcl.mmatomax_int
     ./translator/sbcl.mmatomax
     ./translator/sbcl.mmatomax_int     
                                        );


##############################################################################


# nocopy means don't copy from source to builddir. This
# is done specially. Well they are copied in the list of translator
# files, not bin files
our @Bin_files = qw (
  mixima
  miximamma
  mockmma);
@Bin_files = (@Bin_files,
              { name => 'gcl.mmatomax', nocopy => 1},
              { name => 'gcl.mmatomax_int', nocopy => 1},
              { name => 'sbcl.mmatomax', nocopy => 1},
              { name => 'sbcl.mmatomax_int', nocopy => 1},
          );
                  




our @Doc_files =
    qw (
           NOTES-maxima-developer
           mixima_user_document
   );


our @Top_level_files_requiring_version =
    qw(
          README README.translator function_list
  );

our @someTop_level_and_other_files =
    qw (
           file_lists.pl
           extract_func_info.pl
           mkshadow.pl
           mkdistcompat.pl
           stripc_commments.pl
           mkhelplisp.pl
           ChangeLog
           README.compatibility
           README.plotting
           Authors
           Copyright
           TODO
           BUGS
           copyright.parser
           copyright.rationale.parser
           install.pl
           index.html
           uninstall.sh
   );

our @Top_level_and_other_files = (@someTop_level_and_other_files,@Top_level_files_requiring_version);

# These will be copied to filenames ending in txt so that a browser can read them
# version does not mean version number here !
our @Files_needing_text_versions =  (
     "./tests/rtest_mixima.mac",
     "function_list",
     "README",
     "README.translator",
     "ChangeLog"
 );

# files that will go in the top level of the web archive
our @Webfiles =  (
    "index.html",
#    $Tarname,
#    $Zipname,
    "function_list.txt",
    "./tests/rtest_mixima.txt",
    "ChangeLog.txt",
    "README.txt",
    "README.translator.txt",
);

########################################################

# The compatibility files that are installed into the maxima share directory
our @Compatibility_install_files =
    (@Compatibility_mac_install_files,@Compatibility_lisp_install_files);

# The compatibility files that go into the distribution tarball.
our @Compatibility_files =
    (@Compatibility_install_files, @Compatibility_lisp_install_files, @Compatibility_lisp_files_for_build);

# The translator lisp files that go in the distribution and
# are installed.
our @Translator_dist_files = (@Translator_source_install_files,@Translator_other_files);

# structure of most of the files for distribution

# These files will be put in the distribution
our %Dist_files = (
    'Compatibility' => \@Compatibility_files,
    'Translator' => \@Translator_dist_files,
    'Bin' => \@Bin_files,
    'Tests' => \@Test_files,
#    'Doc' => \@Doc_files,
);

#  These files will be installed in the maxima share directory.
our %Inst_files = (
    'Compatibility' => \@Compatibility_install_files,
    'Translator' => \@Translator_source_install_files,
    'Tests' => \@Test_files
);

our %Version_numbered_files = (
   Compatibility => $Dist_files{Compatibility},
   Tests => $Dist_files{Tests},
   Translator => \@Translator_source_files_requiring_versions,
#   Top => \@Top_level_files_requiring_version,
);


#########################################

# get the source and target directory from a name
sub get_src_targ_dirs {
    my ($main_dir_name) = @_;
    die "Unknown name $main_dir_name" unless exists
        $Main_dirs{$main_dir_name} and exists  $Dist_main_dirs{$main_dir_name};
    return ( $Main_dirs{$main_dir_name}, $Dist_main_dirs{$main_dir_name} );
}


# This tries to find the maxima executable, then tries to find
# the top level share directory.
# It seems like 'maxima -d' might be a better choice'
sub find_share_dir {
    my $executable = `/bin/sh -c 'type maxima'`;
    $executable =~ /([^\s]+)$/;
    $executable = $1;
    if (not $executable ) {
	    die "Unable to find maxima executable";
	}
    my $instdirs = `$executable -r 'file_search_maxima;'`;
    $instdirs =~ /\[([^\]]+)\]/;
	my @dirs = split(",",$1);
	my $res=0;
	foreach (@dirs) {
	    if( /share/) {
		    $res = $_;
		    last;
		}
	}
	chomp($res);
	$res =~ s/\#.+$//;
	$res =~ s/\n//;
	$res =~ s/^\s+//;
        $res =~ s/\/$//;
	return($res);
}

sub make_mixima_install_dir_name {
    if ( not defined $Install_share_dir or not $Install_share_dir ) {
	$Install_share_dir = find_share_dir();
    }
    if (not $Install_share_dir ) {
	die "Unable to find maxima share directory";
    }
    $Mixima_share_dir = "$Install_share_dir/mixima";
}
