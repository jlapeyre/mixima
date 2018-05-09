#!/usr/bin/perl -w

# This script builds a distribution package of mixima
# The line
# $VERSION="0.xx"; in file_lists.pl should be changed before the script
# is run. The script will delete an existing distribution with
# that number if it exists and create a new one.

use strict;
use File::Copy;
use File::Basename;
use File::Spec::Functions;
require "./file_lists.pl";
require "./config_install.pl";

our (%Main_dirs, %Dist_main_dirs, $Dist_top_dir, @Files_needing_text_versions,
     @Top_level_and_other_files, %Dist_files, $Tarname, $Zipname, $Webdir, @Webfiles,
      $VERSION);

our (@Translator_source_files_requiring_versions,@Top_level_files_requiring_version);

our (%Version_numbered_files);

our @Files_needing_mixima_install_path;
our $Mixima_share_dir;
our $Build_dir;
our $Lisp_interpreter_for_scripts;


sub deb {my $c = shift; print STDERR "$c\n";}
sub dosys {
    my ($c) = @_;
    print $c,"\n";
    system $c;
    if ( $?  != 0 ) {
        die "*** Command '$c' failed.";
    } 
}

sub rmtree {
    my ($dir) = @_;
    die "Can't remove tree $dir" unless -d $dir;
    dosys("rm -rf $dir");
}

sub dmkdir {
    my ($dir) = @_;
    dosys("mkdir $dir");
}

sub dmkdir_time_stamp {
    my ($src,$targ) = @_;
    die "Can't find directory '$src'" unless -d $src;
    dosys("mkdir $targ");
    dosys("touch -r $src $targ");
}

sub make_main_dist_dirs {
    print "------ Making top level directories\n";
    if ( -e $Dist_top_dir ) {
        print STDERR "$Dist_top_dir alredy exists. removing.\n";
        rmtree($Dist_top_dir);
    }
    dmkdir($Dist_top_dir);
    foreach my $n ( keys %Main_dirs ) {
        next if $n eq 'Top';
        next if $n eq 'Doc';
        dmkdir_time_stamp( get_src_targ_dirs($n) );
    }
}

sub prepare_files {
    print "------ Preparing files\n";
    dosys("./mkshadow.pl");
    dosys("./mkhelplisp.pl >  compatibility/mixdoc.lisp");
    dosys("./extract_func_info.pl");
}

# this routine still needs to be rewritten because there
# should be no explicit filenames used as data here.
sub install_applications {
    print "------ Installing applications\n";
    my @qinf_griffiths = qw (
      README.md qinf050.ma qinf050_mod.m  diff_qinf050_modqinf qinf050_mod_raw.mac
     qinf050.mac qinf_byhand.mac rtest_qinf.mac  misc10.ma misc10_raw.mac
      misc10_mod.mac misc10_reformatted.mac rtest_misc10.mac 
     );
    my $qinfdir = "./applications/qinf_griffiths";
    dosys("install -p -d $Dist_top_dir/$qinfdir");
    dosys("touch -r $qinfdir $Dist_top_dir/$qinfdir");
    foreach(@qinf_griffiths) {
	dosys("cp -a $qinfdir/$_  $Dist_top_dir/$qinfdir/$_");
    }
    dosys("touch -r ./applications  $Dist_top_dir/applications");
    dosys("touch -r $qinfdir $Dist_top_dir/$qinfdir");
}


sub make_text_versions {
    print "------ Adding txt to some www files\n";
    foreach (@Files_needing_text_versions) {
	my $tf = $_;
	$tf =~ s/\.mac//;
	$tf =~ s/\.lisp//;
	$tf .= ".txt";
	dosys("cp -a $_  ./$tf");
    }
#    dosys("mv ./tests/*.txt .."); # this will be installed from where it is
}

sub copy_top_files {
    print "------ Installing top level files\n";
    foreach (@Top_level_and_other_files) {
	dosys("install -p -D  $_ $Dist_top_dir/$_");
    }
}

sub copy_main_files {
    foreach my $name (keys %Dist_files ) {
        my $files = $Dist_files{$name};
        my ($src,$targ) = get_src_targ_dirs($name);
        print "------ Installing $name files\n";
        foreach (@$files) {
#            print "ref --> " . ref($_) . "\n";
            next  if  ref($_) eq 'HASH' and exists $_->{nocopy};
            dosys("install -p  $src/$_ $targ/$_");
        }
    }
}


# filenames belong in file_lists.pl
sub copy_translator_files {
    print "------ Installing translator subdirs\n";
    my ($src,$targ) = get_src_targ_dirs('Translator');
    foreach my $exdir (qw( mma_examples our_mma_tests )) {
	dosys("mkdir $targ/$exdir");
	dosys("cp -a $src/$exdir/*.m $targ/$exdir");
	dosys("cp -a $src/$exdir/*.ex.mac $targ/$exdir");
    }
}


# substitute version numbers
sub more_substitutions {
    print "------ Making version number substitutions\n";
    foreach my $name (keys %Version_numbered_files ) {
        my $files = $Version_numbered_files{$name};
        my ($src,$targ) = get_src_targ_dirs($name);
        print "------ Writing versions in  $name files\n";
        foreach (@$files) {
            substitute_version("$targ/$_");
        }
    }
}

sub mktar {
    print "------ Making tar file\n";
    dosys("tar czf $Tarname $Dist_top_dir");
}

sub mkzip {
    print "------ Making zip file\n";
    dosys("rm $Zipname; zip -r $Zipname $Dist_top_dir &> /dev/null");
}

sub mkweb {
    print "------ Installing www files\n";
    dosys("rm -rf $Webdir; install -p -d $Webdir");
    foreach (@Webfiles) {
	dosys("cp -a $_ ./$Webdir");
    }
}

sub do_version_subs {
    print "------ Doing versions substitutions\n";
    my @files = qw( index.html ChangeLog.txt
     function_list.txt README.txt rtest_mixima.txt);
    foreach(@files) {
	substitute_version("$Webdir/$_");
    }
}

# There is extra crap in here, but it works.
# copy a file, substituting version number $Version 
# for the string $versionstring
sub substitute_version {
    my ($infilename, $Version, $versionstring, $opts) = @_;
    my ($outfilename);
    $Version = $VERSION unless defined $Version;
    $$opts{version} = 1 unless exists $$opts{version};
    $versionstring = "VERSIONNUMBER" unless defined $versionstring;
    $outfilename = $infilename;
    $outfilename .= ".out";
    local(*IH);
    local(*OH);
    print "substitue version: Reading and writing $infilename, '$outfilename'\n";
    open(IH,"<$infilename")  or die "Can't open $infilename for reading";
    open(OH,">$outfilename") or die "Can't open $outfilename for writing";
    while ( $_ = <IH> ) {
        s/$versionstring/$Version/;
	print OH or die "Failed to write to $outfilename";
#   for debugging
#	if ( s/$versionstring/$Version/ ) {
#	    print STDERR "**************REPLACING $versionstring with $Version\n";
#	    print STDERR "** Altered line **\n";
#	    print STDERR "** '$_'\n";
#	}
    }
    close(IH);
    close(OH);
    dosys("touch -r $infilename $outfilename"); # timestamp
    dosys("mv -f $outfilename $infilename");
}

# (setq mixima-installation-path "./")
sub write_installation_path_in_scripts {
    make_mixima_install_dir_name();
    print "------ Writing installation path in scripts\n";
    my $lisp_path_match = "\(setq mixima-installation-path[^\)]+\)";
    my $path_line = "setq mixima-installation-path \"$Mixima_share_dir\"";
    foreach my $filepath ( @Files_needing_mixima_install_path ) {
        my ($file,$dir) = fileparse($filepath);
        my $file_bak = $file . ".bak";
        my $newpath = catfile($Build_dir , $dir, $file);
        my $bakpath = catfile($Build_dir , $dir, $file_bak);
        my $binpath = catfile($Build_dir, $Main_dirs{Bin}, $file);
        copy($newpath, $bakpath) or die
                    "copy($newpath, $bakpath) failed";
        open my $inh , '<', $bakpath or die $!;
        die "Can't find copied file $bakpath" unless -e $bakpath;
        local $/; # enable localized slurp mode
        my $script_text = <$inh>;
        die "Can't read script $bakpath" unless defined $script_text and $script_text;
        if ( $script_text =~ s/$lisp_path_match/$path_line/ ) {
            1;
        }
        else {
            die "Unable to find line in script $file_bak to put mixima installation path.";
        }
        open my $outh , '>', $newpath or die $!;
        print $outh $script_text;
        close $outh;
        if ( $file =~ /^(gcl|sbcl)/) {
            next unless $Lisp_interpreter_for_scripts eq $1;
            my $sfile = $file;
            $sfile =~ s/^(gcl|sbcl)\.//;
            my $sbinpath = catfile($Build_dir, $Main_dirs{Bin}, $sfile);
            print "Copying $newpath to $sbinpath\n";
            copy($newpath,$sbinpath) or die "Copy copy($newpath,$sbinpath) failed";
        }
        else {
            print "Copying $newpath to $binpath\n";
            copy($newpath,$binpath) or die "Copy copy($newpath,$binpath) failed";
        }
    }
    print "------ Done Writing installation path in scripts\n";    
}

sub do_final_touch {
    foreach my $dir (qw( compatibility bin tests translator )) {
        dosys "touch -r ./$dir $Dist_top_dir/$dir";
    }
}


make_main_dist_dirs();
prepare_files();
make_text_versions();
#copy_top_files();
copy_main_files();
copy_translator_files();
install_applications();
more_substitutions();
#mktar();
#mkzip();
#mkweb();
#do_version_subs();
write_installation_path_in_scripts();
do_final_touch();
