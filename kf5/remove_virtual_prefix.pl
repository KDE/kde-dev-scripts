#!/usr/bin/perl -w

# Usage: remove_virtual_prefix.pl *.h 
# remove virtual when we have Q_DECL_OVERRIDE

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    # I don't use functionUtilkde::substInFile because it touches all files, even those which were not modified.
    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        my $regexp = qr/
           ^(\s*)                        # (1) Indentation
           virtual\s*                    # (2) virtual
           (.*)                          # (3) function
           Q_DECL_OVERRIDE(.*)$
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $function, $end) = $_ =~ $regexp) {
           $_ = $indent . $function . "Q_DECL_OVERRIDE" . $end . "\n";
        }
 
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
