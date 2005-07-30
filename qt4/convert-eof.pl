#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function convert .eof() to atEnd()

use lib qw( . );
use functionUtilkde; 


foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
	s!.eof\s*\(\s*\)!.atEnd()!;
    } $file;
}
functionUtilkde::diffFile( "@ARGV" );

