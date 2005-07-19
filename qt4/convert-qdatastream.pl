#!/usr/bin/perl 

# Montel Laurent <montel@kde.org>
# This script allows to convert QDataStream to new API

use lib qw( . );
use functionUtilkde; 

foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
	if (my ($blank, $prefix, $contenu) = m!^(\s*)(.*QDataStream.*)\((.*)\s*\);$!) {
	    if ( my ($firstelement, $secondelement) = m!.*\(\s*(.*),\s*(.*)\);\s*$!) {
		if ( $prefix !~ /operator/ && substr($firstelement, 0, 1) ne "&") 
		{
		    $_ = $blank . $prefix . "( &" . $firstelement . "," . $secondelement . ");\n" ;
		}
	    }
	}
	
    } $file;

}
functionUtilkde::diffFile( "@ARGV" );
