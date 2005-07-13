#!/usr/bin/perl 


# laurent Montel <montel@kde.org>
# this function add q3tl.h include necessary for qheadsort function

use lib qw( . );
use functionUtilkde; 

foreach my $file (@ARGV) {
   local *F;
   open F, "+<", $file or do { print STDOUT "open($file) failled : \"$!\"\n"; next };
   my $str = join '', <F>;
   $str =~ s!(#include <.*#include <[^
]*)!\1\n#include <q3tl.h>!smig;
    seek F, 0, 0;
    print F $str;
    truncate F, tell(F);
}
functionUtilkde::diffFile( "@ARGV" );
