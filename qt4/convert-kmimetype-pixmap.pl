#!/usr/bin/perl

# David Faure <faure@kde.org> 2007
# Based on script by Laurent Montel <montel@kde.org> 2005
# This script converts KMimeType::pixmap (deprecated) to KIconLoader::loadMimeTypeIcon
#        and KDEDesktopMimeType:: to KDesktopFileActions::

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;

foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
	s/(KMimeType::mimeType\([^)]*\))->pixmap\(([^)]*)\)/KIconLoader::global()->loadMimeTypeIcon($1->iconName(),$2)/sg;
	# the regexp below is a bit too greedy, it also matches fileitem->pixmap()
	#s/(\w*)->pixmap\(([^)]*)\)/KIconLoader::global()->loadMimeTypeIcon($1->iconName(),$2)/sg;
	s/KDEDesktopMimeType::/KDesktopFileActions::/g;
	s/kdedesktopmimetype\.h/kdesktopfileactions.h/g;
    } $file;
}

functionUtilkde::diffFile( "@ARGV" );

