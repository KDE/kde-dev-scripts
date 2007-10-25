#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2007 GPL
# This script will remove not necessary cmake variable (already defined by dependancy)
#

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

open(my $F, q(find -name "CMakeLists.txt" |));
my $file;
my $warning;
while ($file = <$F>) {
    chomp $file;
    next if functionUtilkde::excludeFile( $file);
    my $modified;
    open(my $FILE, $file) or warn "We can't open file $file:$!\n";
    my @l = map {
	my $orig = $_;

	if( /KDE4_KWALLETCLIENT_LIBS/ )
	{
		s!\${/KDE4_KWALLETCLIENT_LIBS} !!;
	}
	if( /KDE4_KSPELL2_LIBS/ )
	{
		s!\${KDE4_KSPELL2_LIBS} !!;
	}
        if( /KDE4_KDEPRINT_LIBS/ )
        {
                s!\${KDE4_KDEPRINT_LIBS} !!;
        }
    	if ( /target_link_libraries/ or /TARGET_LINK_LIBRARIES/) {
                if( $_ =~ m/\${KDE4_KDECORE_LIBS}/ and $_ =~ m/\${KDE4_KDEUI_LIBS}/ )
                {
                        s!\${KDE4_KDECORE_LIBS} !!;
                }
		if( $_ =~ m/\${KDE4_KIO_LIBS}/ and $_ =~ m/\${KDE4_KDEUI_LIBS}/ )
		{
			s!\${KDE4_KDEUI_LIBS} !!;
		}
		# dependancy for kfile
                if( $_ =~ m/\${KDE4_KFILE_LIBS}/ and $_ =~ m/\${KDE4_KDEUI_LIBS}/ )
                {
                        s!\${KDE4_KDEUI_LIBS} !!;
                }
                if( $_ =~ m/\${KDE4_KFILE_LIBS}/ and $_ =~ m/\${KDE4_KDECORE_LIBS}/ )
                {
                        s!\${KDE4_KDECORE_LIBS} !!;
                }                
		if( $_ =~ m/\${KDE4_KFILE_LIBS}/ and $_ =~ m/\${KDE4_KIO_LIBS}/ )
                {
                        s!\${KDE4_KIO_LIBS} !!;
                }
                if( $_ =~ m/\${KDE4_KFILE_LIBS}/ and $_ =~ m/\${KDE4_SOLID_LIBS}/ )
                {
                        s!\${KDE4_SOLID_LIBS} !!;
                }
 		# dependancy for kdesu
                if( $_ =~ m/\${KDE4_KDESU_LIBS}/ and $_ =~ m/\${KDE4_KDECORE_LIBS}/ )
                {
                        s!\${KDE4_KDESU_LIBS} !!;
                }
		# dependancy for kparts
		if( $_ =~ m/\${KDE4_KPARTS_LIBS}/ and $_ =~ m/\${KDE4_KDECORE_LIBS}/ )
                {
                        s!\${KDE4_KDECORE_LIBS} !!;
                }
                if( $_ =~ m/\${KDE4_KPARTS_LIBS}/ and $_ =~ m/\${KDE4_KDEUI_LIBS}/ )
                {
                        s!\${KDE4_KDEUI_LIBS} !!;
                }
                if( $_ =~ m/\${KDE4_KPARTS_LIBS}/ and $_ =~ m/\${KDE4_KIO_LIBS}/ )
                {
                        s!\${KDE4_KIO_LIBS} !!;
                }
		#dependancy for knewstuff2
                if( $_ =~ m/\${KDE4_KNEWSTUFF2_LIBS}/ and $_ =~ m/\${KDE4_KIO_LIBS}/ )
                {
                        s!\${KDE4_KIO_LIBS} !!;
                }
                if( $_ =~ m/\${KDE4_KNEWSTUFF2_LIBS}/ and $_ =~ m/\${KDE4_KDECORE_LIBS}/ )
                {
                        s!\${KDE4_KDECORE_LIBS} !!;
                }
                if( $_ =~ m/\${KDE4_KNEWSTUFF2_LIBS}/ and $_ =~ m/\${KDE4_KDEUI_LIBS}/ )
                {
                        s!\${KDE4_KDEUI_LIBS} !!;
                }
		#dependancy for kutils
                if( $_ =~ m/\${KDE4_KUTILS_LIBS}/ and $_ =~ m/\${KDE4_KDECORE_LIBS}/ )
                {
                        s!\${KDE4_KDECORE_LIBS} !!;
                }

                if( $_ =~ m/\${KDE4_KUTILS_LIBS}/ and $_ =~ m/\${KDE4_KDEUI_LIBS}/ )
                {
                        s!\${KDE4_KDEUI_LIBS} !!;
                }
		#dependancy for khtml
		if( $_ =~ m/\${KDE4_KHTML_LIBS}/ and $_ =~ m/\${KDE4_KDECORE_LIBS}/ )
		{
			s!\${KDE4_KDECORE_LIBS} !!;
		}
                if( $_ =~ m/\${KDE4_KHTML_LIBS}/ and $_ =~ m/\${KDE4_KIO_LIBS}/ )
                {
                        s!\${KDE4_KIO_LIBS} !!;
                }
                if( $_ =~ m/\${KDE4_KHTML_LIBS}/ and $_ =~ m/\${KDE4_KDEUI_LIBS}/ )
                {
                        s!\${KDE4_KDEUI_LIBS} !!;
                }
                if( $_ =~ m/\${KDE4_KHTML_LIBS}/ and $_ =~ m/\${KDE4_KPARTS_LIBS}/ )
                {
                        s!\${KDE4_KPARTS_LIBS} !!;
                }
		#dependancy for ktexteditor
                if( $_ =~ m/\${KDE4_KTEXTEDITOR_LIBS}/ and $_ =~ m/\${KDE4_KDECORE_LIBS}/ )
                {
                        s!\${KDE4_KDECORE_LIBS} !!;
                }
                if( $_ =~ m/\${KDE4_KTEXTEDITOR_LIBS}/ and $_ =~ m/\${KDE4_KPARTS_LIBS}/ )
                {
                        s!\${KDE4_KPARTS_LIBS} !!;
                }
		#dependancy for KDE4_KPIMIDENTITIES_LIBS
		if( $_ =~ m/\${KDE4_KPIMIDENTITIES_LIBS}/ and $_ =~ m/\${KDE4_KDECORE_LIBS}/ )
                {
                        s!\${KDE4_KDECORE_LIBS} !!;
                }
                if( $_ =~ m/\${KDE4_KPIMIDENTITIES_LIBS}/ and $_ =~ m/\${KDE4_KDEUI_LIBS}/ )
                {
                        s!\${KDE4_KDEUI_LIBS} !!;
                }
                if( $_ =~ m/\${KDE4_KPIMIDENTITIES_LIBS}/ and $_ =~ m/\${KDE4_KIO_LIBS}/ )
                {
                        s!\${KDE4_KIO_LIBS} !!;
                }
		#dependancy for KDE4_KDNSSD_LIBS
		if( $_  =~ m/\${KDE4_KDNSSD_LIBS}/ and $_ =~ m/\${KDE4_KDECORE_LIBS}/ )
                {
                        s!\${KDE4_KDECORE_LIBS} !!;
                }
                if( $_  =~ m/\${KDE4_KDNSSD_LIBS}/ and $_ =~ m/\${KDE4_KDEUI_LIBS}/ )
                {
                        s!\${KDE4_KDEUI_LIBS} !!;
                }

		# Not remove them it will necessary when we removed kde3support into apps.
		#if( $_ =~ m/\${KDE4_KDE3SUPPORT_LIBS}/ and $_ =~ m/\${KDE4_KDECORE_LIBS}/ )
		#{
		#	s!\${KDE4_KDECORE_LIBS} !!;
		#}
		#if( $_ =~ m/\${KDE4_KDE3SUPPORT_LIBS}/ and $_ =~ m/\${KDE4_KDECORE_LIBS}/ )
		#{
		#        s!\${KDE4_KDECORE_LIBS} !!;
		#}
		#if( $_ =~ m/\${KDE4_KDE3SUPPORT_LIBS}/ and $_ =~ m/\${KDE4_KDEUI_LIBS}/ )
		#{
		#        s!\${KDE4_KDEUI_LIBS} !!;
		#}
		#if( $_ =~ m/\${KDE4_KDE3SUPPORT_LIBS}/ and $_ =~ m/\${KDE4_KIO_LIBS}/ )
		#{
		#        s!\${KDE4_KIO_LIBS} !!;
		#}
		#if( $_ =~ m/\${KDE4_KDE3SUPPORT_LIBS}/ and $_ =~ m/\${KDE4_KPARTS_LIBS}/ )
		#{
		#        s!\${KDE4_KPARTS_LIBS} !!;
		#}
	}
	$modified ||= $orig ne $_;
	$_;
    } <$FILE>;

    if ($modified) {
	open (my $OUT, ">$file");
	print $OUT @l;
    }

}
functionUtilkde::diffFile( <$F> );
