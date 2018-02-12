#!/usr/bin/perl -w

# Usage: add_destructor_override.pl *.h 
# add overide when we have virtual destructor (not perfect as sometimes we define destructor as virtual.
# so you need to test compile.

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
           virtual\s*~                   # (2) virtual
           (.*)                          # (3) function
           \(\);$
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $function) = $_ =~ $regexp) {
           $_ = $indent . "~" . $function . "() override;" . "\n";
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
