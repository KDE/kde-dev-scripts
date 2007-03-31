#!/usr/bin/perl 


# laurent Montel <montel@kde.org>
# this function add q3tl.h include necessary for qheadsort function

use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {
		functionUtilkde::addIncludeInFile( $file, "q3tl.h");
}
functionUtilkde::diffFile( "@ARGV" );
