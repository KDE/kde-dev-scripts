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
    my @l = map {
        my $orig = $_;

        #KSaveFile file( filename );

        my $regexp = qr/
          ^(.*?)           # (1) Indentation, possibly "Classname *" (the ? means non-greedy)
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
           }
        }
        # Add default argument
        if (/(\w+)\.open\(\s*\)/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
              s/$var\.open\(\s*\)/$var\.open\(QIODevice::ReadWrite\)/;
           }
        }
        s/\bKSaveFile\b/QSaveFile/g;
        s/\<KSaveFile\b\>/\<QSaveFile>/ =~ /#include/ ;
        s/\<ksavefile.h\>/\<QSaveFile>/ =~ /#include/ ;
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
