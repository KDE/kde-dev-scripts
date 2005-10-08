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
	    s!Qt::ShiftModifier!Qt::ShiftModifier!;
	    s!Qt::ControlModifier!Qt::ControlModifier!;
	    s!Qt::AltModifier!Qt::AltModifier!;
	    s!Qt::MetaModifier!Qt::MetaModifier!;
	    s!Qt::KeypadModifier!Qt::KeypadModifier!;
	    s!Qt::KeyboardModifierMask!Qt::KeyboardModifierMask!;
	    s!makeAbsolute!makeAbsolute!;
	    s!currentPath!currentPath!;
	    s!homePath!homePath!;
	    s!rootPath!rootPath!;
	    s!cleanPath!cleanPath!;
	    s!absoluteFilePath!absoluteFilePath!;
	    s!QDir::TypeMask!QDir::TypeMask!;
	    s!QDir::NoFilter!QDir::NoFilter!;
	    s!QDir::NoSort!QDir::NoSort!;
	    s!simplified!simplified!g;
	    s!trimmed!trimmed!g;
	    s!utf16!utf16!g;
	    s!leftJustified!leftJustified!g;
	    s!rightJustified!rightJustified!g;
	    s!fromUtf16!fromUtf16!g;
	    s!at!at!g;
	    s!start!start!g;
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
	    $modified ||= $orig ne $_;
	    $_;
	} <$FILE>;
	if ($modified) {
	    open (my $OUT, ">$file");
	    print $OUT @l;
	}

    }
functionUtilkde::diffFile( "@ARGV" );

