#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2005
# This script will remove some QT3_SUPPORT into qstring/qdir/qtimer/qglobal

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
			s!local8Bit!toLocal8Bit!g;
			if( $_ =~ /\.utf8/ ) {
				s!\.utf8!\.toUtf8!g;
			}	
			if( $_ =~ /\.mirrored/ ) {
				s!mirrored!hasMirrored!g;
			}
			if( $_ =~ /absPath\s*\(\s*\)/ ) {
					s!absPath!absolutePath!;
			}
			if ( $_ =~ /\.lower\s*\(\s*\)/ ) {
					s!\.lower!\.toLower!g;
			}
            if ( $_ =~ /\.upper\s*\(\s*\)/ ) {
                    s!\.upper!\.toUpper!g;
            }			
			if( $_ =~ /dirPath/ ) {
					if( my ($before, $prefix, $contenu, $after ) = m!^(\s*.*)(dirPath.*?\()(.*?\))(.*)$!) {
							$contenu =~ s/ //g;
							$contenu =~ s/\)//;
							if( $contenu =~ /true|TRUE/ ) {
									$_ = $before . "absolutePath()" . $after . "\n";
							}
							elsif ( $contenu =~ /false|FALSE/ ) {
									$_ = $before . "path()" . $after . "\n";
							}
							elsif ($contenu eq "" ) {
									$_ = $before . "path()" . $after . "\n";
							}
							else {
									warn "Verify if we can port or not : <$contenu> \n";
							}
					}	
			}
			s!Qt::ShiftButton!Qt::ShiftModifier!;
			s!Qt::ControlButton!Qt::ControlModifier!;
			s!Qt::AltButton!Qt::AltModifier!;
			s!Qt::MetaButton!Qt::MetaModifier!;
			s!Qt::Keypad!Qt::KeypadModifier!;
			s!Qt::KeyButtonMask!Qt::KeyboardModifierMask!;
			s!convertToAbs!makeAbsolute!;
			s!currentDirPath!currentPath!;
			s!homeDirPath!homePath!;
			s!rootDirPath!rootPath!;
			s!cleanDirPath!cleanPath!;
			s!absFilePath!absoluteFilePath!;
			s!QDir::All!QDir::TypeMask!;
			s!QDir::DefaultFilter!QDir::NoFilter!;
			s!QDir::DefaultSort!QDir::NoSort!;
			s!simplifyWhiteSpace!simplified!g;
			s!stripWhiteSpace!trimmed!g;
			s!ucs2!utf16!g;
			s!leftJustify!leftJustified!g;
			s!rightJustify!rightJustified!g;
			s!fromUcs2!fromUtf16!g;
			s!constref!at!g;
			s!changeInterval!start!g;
    } $file;

}
functionUtilkde::diffFile( "@ARGV" );

