#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function convert .eof() to atEnd()

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;

foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
	s!.eof\s*\(\s*\)!.atEnd()!;
    } $file;
}
functionUtilkde::diffFile( "@ARGV" );

