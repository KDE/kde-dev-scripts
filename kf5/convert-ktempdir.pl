#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KTempDir->QTemporaryDir
# find -iname "*.cpp" -o -iname "*.h" |xargs kde-dev-scripts/kf5/convert-ktempdir.pl
# TODO need to improve it.
use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;


foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my %varname = ();
    my %finalized_called = ();
    my @l = map {
        my $orig = $_;

        my $regexpKTempDirLocal = qr/
          ^(\s*)           # (1) Indentation
          KTempDir\s+
          (\w+)            # (2) variable name
          /x; # /x Enables extended whitespace mode
        if (my ($left, $var) = $_ =~ $regexpKTempDirLocal) {
           $varname{$var} = 1;
           s/\bKTempDir\b/QTemporaryDir/;
           warn "Be sure that you create temporary dir. if you don't use default constructor\n";
        }

        my $regexp = qr/
           ^(\s*)                        # (1) Indentation, possibly "Classname *" (the ? means non-greedy)
           (.*?)                         # (2) Possibly "Classname *" (the ? means non-greedy)
           (\w+)                         # (3) variable name
           \s*=\s*                       #   assignment
           new\s+KTempDir\s*\((.*)\)        # (4)  new KTempDir(...,...,...,...);
           (.*)$                         # (5) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $var, $argument, $afterreg) = $_ =~ $regexp) {
          warn "KTempDir found \'$var\' \'$argument\'\n";
           $varname{$var} = 1;
           s/\bKTempDir\b/QTemporaryDir/;
           warn "Be sure that you create temporary dir. if you don't use default constructor\n";
        }

        my $regexpKTempDirFunction = qr/
          (.*?)
          KTempDir\s*\*
          (\w+)            # (2) variable name
          /x; # /x Enables extended whitespace mode
        if (my ($left, $var) = $_ =~ $regexpKTempDirFunction) {
           warn "Found KTempDir in function!!!! $var\n";
           $varname{$var} = 1;
           s/\bKTempDir\b/QTemporaryDir/;
           warn "Be sure that you create temporary dir. if you don't use default constructor\n";
        }

        if (/(\w+)\.name\(/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
              s/$var\.name/$var\.path/;
           }
        }

        if (/(\w+)\->name\(/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
              s/$var\->name/$var\->path/;
           }
        }

        if (/(\w+)\.unlink\(/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
              s/$var\.unlink/$var\.remove/;
           }
        }

        if (/(\w+)\.exists\(/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
              warn "$file: replace exists by QTemporaryDir::isValid() + QDir::exists(path) if necessary\n";
              s/$var\.exists/$var\.isValid/;
           }
        }
        if (/(\w+)\->exists\(/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
              warn "$file: replace exists by QTemporaryDir::isValid() + QDir::exists(path) if necessary\n";
              s/$var\->exists/$var\->isValid/;
           }
        }


        if (/(\w+)\->unlink\(/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
              s/$var\->unlink/$var\->remove/;
           }
        }
        my $regexpRemoveDir = qr/
           ^(\s*)                         # (1) Indentation, possibly "Classname *" (the ? means non-greedy)
           (.*?)                          # (2) before KTempDir::removeDir
           KTempDir::removeDir
           ${functionUtilkde::paren_begin}3${functionUtilkde::paren_end}    # (3) (args)
           (.*)$                         # (4) afterreg
           /x; # /x Enables extended whitespace mode
        if ( my ($indent, $before, $argument, $after) = $_ =~ $regexpRemoveDir) {
           $_ = $indent . $before . "QDir($argument).removeRecursively()" . $after . "\n";
        } 

        s/\bKTempDir\b/QTemporaryDir/g;
        s/\<KTempDir\b\>/\<QTemporaryDir>/ if (/#include/);
        s/\<ktempdir.h\>/\<QTemporaryDir>/ if (/#include/);
        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
    }
}

functionUtilkde::diffFile( "@ARGV" );
