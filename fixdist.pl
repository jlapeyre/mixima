#!/usr/bin/perl

# bzr only exports the distribution. we want to add
# other files

$orig_tar_file = $ARGV[0];

$orig_dir = $orig_tar_file;
$orig_dir =~ s/\/[^\/]+$//;

$tar_dir = $orig_tar_file;
$tar_dir =~ s/\.tar\.gz//;
$orig_tar_file =~ /^.+\/([^\/]+)$/;
$tar_file_short = $1;

$zip_file_short = $tar_file_short;
$zip_file_short =~ s/tar\.gz/zip/;

$tar_dir_short = $tar_file_short;
$tar_dir_short =~ s/\.tar\.gz//;

sub dosys {
    my ($c) = @_;
    print STDERR $c, "\n";
    system $c;
}

sub cddo {
    my ($c) = @_;
    dosys("cd $orig_dir; $c");
}

my $bd = "./build";
dosys("mkdir $bd/mixima");
dosys("cp -a $bd/compatibility/* $bd/mixima");
dosys("cp -a $bd/translator/* $bd/mixima");
dosys("cp -a $bd/tests/* $bd/mixima");
dosys("rm -r $bd/web");
cddo("tar xzf $tar_file_short");
dosys("cp -a $bd $tar_dir");
dosys("mv $orig_tar_file $orig_tar_file.old");
cddo("tar czf $tar_file_short  $tar_dir_short");
cddo("zip -q -r $zip_file_short $tar_dir_short");
