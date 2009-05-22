#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2005
# This script will remove some QT3_SUPPORT into qstring/qdir/qtimer/qglobal

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

open(my $F, q(find -name "*" |));
my $file;
while ($file = <$F>) {
 	chomp $file;
	next if functionUtilkde::excludeFile( $file);
	open(my $FILE, $file) or warn "We can't open file $file:$!\n";
	my $modified;
	my @l = map {
	    my $orig = $_;
	    if ( $_ =~ /lastIndexOf/ ) {
		# three argument
		my ($before, $prefix, $contenu) = m!^(\s*.*)(lastIndexOf.*)\((.*)\s*\);$!;
		if ( my ($firstelement, $secondelement, $thirdelement) = m!.*?\(\s*(.*),\s*(.*),\s*(.*)\);\s*$!) {
		    $thirdelement =~ s/ //g;
		    if ( $thirdelement =~ /true/ ) {
			$_ = $before . "lastIndexOf( $firstelement, $secondelement, Qt::CaseSensitive );\n";
		    }
		    elsif ( $thirdelement =~ /false/ ) {
			$_ = $before . "lastIndexOf( $firstelement, $secondelement, Qt::CaseInsensitive );\n";
		    }
		    else {
			s/lastIndexOf/lastIndexOf/;
		    }
		}
		else {
		    s/lastIndexOf/lastIndexOf/;	
		}
	    }	
	    s!toLocal8Bit!toLocal8Bit!g;
	    if( $_ =~ /\.toUtf8/ ) {
		s!\.toUtf8!\.toUtf8!g;
	    }	
	    if( $_ =~ /\.hasMirrored/ ) {
		s!mirrored!hasMirrored!g;
	    }
	    if( $_ =~ /absPath\s*\(\s*\)/ ) {
		s!absPath!absolutePath!;
	    }
        if ( $_ =~ /\.latin1\s*\(\s*\)/ ) {
                s!\.latin1!\.toLatin1!g;
        }
		if ( $_ =~ /\.xForm\s*\(/ ) {
			s!\.xForm!\.transformed!;
		}
        if ( $_ =~ /->xForm\s*\(/ ) {
            s!\->xForm!\->transformed!;
        }
	   	s!writeBlock!write!;
		s!readBlock!read!;
		s!isSequentialAccess!isSequential!;

		s!QComboBox::Policy!QComboBox::InsertPolicy!g;
		s!insertionPolicy!insertPolicy!;
		s!setInsertionPolicy!setInsertPolicy!;
		s!NoInsertion!NoInsert!;
		
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

	s!QApplication::reverseLayout!QApplication::isRightToLeft!;
		
		s!Qt::ShiftButton!Qt::ShiftModifier!;
        s!Qt::ControlButton!Qt::ControlModifier!;
        s!Qt::AltButton!Qt::AltModifier!;
        s!Qt::MetaButton!Qt::MetaModifier!;
        s!Qt::Keypad\b!Qt::KeypadModifier!;
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


		s!Q_INT8!qint8!g;
		s!Q_UINT8!quint8!g;
		s!Q_INT16!qint16!g;
		s!Q_UINT16!quint16!g;
        s!Q_INT32!qint32!g;
        s!Q_UINT32!quint32!g;	
        s!Q_INT64!qint64!g;
        s!Q_UINT64!quint64!g;
		s!Q_LLONG!qint64!g;
		s!Q_ULLONG!quint64!g;
		s!QMAX!qMax!g;
		s!QMIN!qMin!g;
		s!Qt::ScaleMin!Qt::KeepAspectRatio!g;
		s!Qt::ScaleMax!Qt::KeepAspectRatioByExpanding!g;
		s!Qt::ScaleFree!Qt::IgnoreAspectRatio!g;
		s!Qt::AlignAuto!Qt::AlignLeft!g;
		s!Qt::CustomPattern!Qt::TexturePattern!g;
        if( $_ =~ /Qt::TopLeft/ ) {
            s!Qt::TopLeft!Qt::TopLeftCorner!g if( $_ !~ /Qt::TopLeftCorner/ );
        }
        if( $_ =~ /Qt::TopRight/ ) {
            s/Qt::TopRight/Qt::TopRightCorner/g if( $_ !~ /Qt::TopRightCorner/ );
        }
        if( $_ =~ /Qt::BottomLeft/ ) {
            s/Qt::BottomLeft/Qt::BottomLeftCorner/g if( $_ !~ /Qt::BottomLeftCorner/);
        }
        if( $_ =~ /Qt::BottomRight/ ) {
            s/Qt::BottomRight/Qt::BottomRightCorner/g if( $_ !~ /Qt::BottomRightCorner/ );
        }

		
		#qcolor.h
		s!setRgba!setRgb!;
		s!getRgba!getRgb!;

		#qscrollbar.h
		s!draggingSlider!isSliderDown!;
		#qtabwidget.h
		s!setTabIconSet!setTabIcon!;
		s!tabIconSet!tabIcon!;

	    $modified ||= $orig ne $_;
	    $_;
	} <$FILE>;
	if ($modified) {
	    open (my $OUT, ">$file");
	    print $OUT @l;
	}

    }
functionUtilkde::diffFile( "@ARGV" );

