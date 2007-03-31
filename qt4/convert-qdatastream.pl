#!/usr/bin/perl 

# Montel Laurent <montel@kde.org>
# This script allows to convert QDataStream to new API

use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
	if (my ($blank, $prefix, $contenu) = m!^(\s*)(.*QDataStream.*)\((.*)\s*\);$!) {
	    if ( my ($firstelement, $secondelement) = m!.*\(\s*(.*),\s*(.*)\);\s*$!) {
		if ( $prefix !~ /operator/ && substr($firstelement, 0, 1) ne "&") 
		{
		    my $argument = $prefix;
		    $argument =~ s/QDataStream\s*//;
		    $_ = $blank . $prefix . "( &" . $firstelement . "," . $secondelement . ");\n" . $blank . $argument . ".setVersion(QDataStream::Qt_3_1);\n";
		}
	    }
	}
	
    } $file;

}
functionUtilkde::diffFile( "@ARGV" );
