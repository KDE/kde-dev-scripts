#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KMenu => QMenu
# find -iname "*.cpp" -o -iname "*.h" |xargs kde-dev-scripts/kf5/convert-kmenu.pl

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
        my $regexpKMenuLocal = qr/
          ^(\s*)           # (1) Indentation
          KMenu\s+
          (\w+)            # (2) variable name
          /x; # /x Enables extended whitespace mode
        if (my ($left, $var) = $_ =~ $regexpKMenuLocal) {
           $varname{$var} = 1;
           s/\bKMenu\b/QMenu/;
        }

        my $regexp = qr/
           ^(\s*)                        # (1) Indentation, possibly "Classname *" (the ? means non-greedy)
           (.*?)                         # (2) Possibly "Classname *" (the ? means non-greedy)
           (\w+)                         # (3) variable name
           \s*=\s*                       #   assignment
           new\s+KMenu\s*\((.*)\)        # (4)  new KMenu(...,...,...,...);
           (.*)$                         # (5) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $var, $argument, $afterreg) = $_ =~ $regexp) {
          warn "KMenu found \'$var\' \'$argument\'\n";
           $varname{$var} = 1;
           s/\bKMenu\b/QMenu/;
        }

        my $regexpKMenuFunction = qr/
          (.*?)
          KMenu\s*\*
          (\w+)            # (2) variable name
          /x; # /x Enables extended whitespace mode
        if (my ($left, $var) = $_ =~ $regexpKMenuFunction) {
           warn "Found KMenu in function!!!! $var\n";
           $varname{$var} = 1;
           s/\bKMenu\b/QMenu/;
        }
 
        if (/(\w+)\.addTitle\(/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
              s/$var\.addTitle/$var\.addSection/;
           }
        }
        if (/(\w+)->addTitle\s*\(/) {
           #warn "found addTitle $1\n";
           my $var = $1;
           if ( defined $varname{$var} ) {
              s/$var\-\>addTitle/$var\-\>addSection/;
           }
        }


        s/\bKMenu\b/QMenu/g;
        s/\<KMenu\b\>/\<QMenu>/ =~ /#include/ ;
        s/\<kmenu.h\>/\<QMenu>/ =~ /#include/ ;

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
