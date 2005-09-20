#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function allows to adapt file to new kdelibs API

use lib qw( . );
use functionUtilkde; 


foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
    }
	s!#include <kaccelmanager.h>!#include <kacceleratormanager.h>!;
	s!KStringHandler::matchFilename!KStringHandler::matchFileName!;
	if ( $_ ~ = /KApplication::random/ ) {
			s!KApplication::random!KRandom::random!;
			functionUtilkde::addIncludeInFile( $file, "krandom.h");
		}
	} $file;
}
functionUtilkde::diffFile( "@ARGV" );

