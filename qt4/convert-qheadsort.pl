#!/usr/bin/perl 


# laurent Montel <montel@kde.org>
# this function add q3tl.h include necessary for qheadsort function

use lib qw( . );
use functionUtilkde; 

foreach my $file (@ARGV) {
		functionUtilkde::addIncludeInFile( $file, "q3tl.h");
}
functionUtilkde::diffFile( "@ARGV" );
