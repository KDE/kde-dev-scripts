#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2005
# This script will remove some QT3_SUPPORT into qstring
# findRev->lastIndexOf
# utf8->toUtf8
# local8bit->toLocal8bit


use lib qw( . );
use functionUtilkde; 

foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
			if ( $_ =~ /findRev/ ) {
				# three argument
				my ($before, $prefix, $contenu) = m!^(\s*.*)(findRev.*)\((.*)\s*\);$!;
				if ( my ($firstelement, $secondelement, $thirdelement) = m!.*?\(\s*(.*),\s*(.*),\s*(.*)\);\s*$!) {
						$thirdelement =~ s/ //g;
						if ( $thirdelement =~ /true/ ) {
								$_ = $before . "lastIndexOf( $firstelement, $secondelement, Qt::CaseSensitive );\n";
						}
						elsif ( $thirdelement =~ /false/ ) {
								$_ = $before . "lastIndexOf( $firstelement, $secondelement, Qt::CaseInsensitive );\n";
						}
						else {
								s/findRev/lastIndexOf/;
						}
				}
				else {
					s/findRev/lastIndexOf/;	
				}
			}	
			s!local8Bit!toLocal8Bit!;
			if( $_ =~ /\.utf8/ ) {
				s!utf8!toUtf8!;
			}	
			s!simplifyWhiteSpace!simplified!;
			s!stripWhiteSpace!trimmed!;
			s!ucs2!utf16!;
			s!leftJustify!leftJustified!;
			s!rightJustify!rightJustified!;
			s!fromUcs2!fromUtf16!;
			s!constref!at!;
    } $file;

}
functionUtilkde::diffFile( "@ARGV" );

