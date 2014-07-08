#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KSaveFile->QSaveFile
# find -iname "*.cpp" -o -iname "*.h" |xargs kde-dev-scripts/kf5/convert-ksavefile.pl
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

        #KSaveFile file( filename );

        my $regexp = qr/
          ^(\s*)           # (1) Indentation
          KSaveFile\s+
          (\w+)            # (2) variable name
          /x; # /x Enables extended whitespace mode
        if (my ($left, $var) = $_ =~ $regexp) {
           $varname{$var} = 1;
           s/\bKSaveFile\b/QSaveFile/;
        }
        if (/(\w+)\.finalize\(/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
              s/$var\.finalize/$var\.commit/;
              $finalized_called{$var} = 1;
           }
        }
        # Add default argument
        if (/(\w+)\.open\(\s*\)/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
              s/$var\.open\(\s*\)/$var\.open\(QIODevice::ReadWrite\)/;
           }
        }
        if (/(\w+)\.abort\(\s*\)/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
              s/$var\.abort\(\s*\)/$var\.cancelWriting\(\)/;
           }
        }

        s/\bKSaveFile\b/QSaveFile/g;
        s/\<KSaveFile\b\>/\<QSaveFile>/ if (/#include/);
        s/\<ksavefile.h\>/\<QSaveFile>/ if (/#include/);
        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    foreach my $var (keys %varname) {
        if (not defined($finalized_called{$var})) {
            warn "WARNING: add $var.commit() at the time it is destructed. KSaveFile called finalize() in the dtor, and this code never calls it, but QSaveFile requires an explicit call to commit(), otherwise the changes are discarded";
         }
    }

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
    }
}

functionUtilkde::diffFile( "@ARGV" );
