#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# this function changes QStringList::split (QT3_SUPPORT) to QString::split (QT4)


use lib qw( /home/lmontel/script-kde/ );
use functionUtilkde; 

foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
    if (my ($blank, $prefix, $contenu) = m!^(\s*.*)(QStringList::split.*)\((.*)\s*\);$!) {
			#warn "blank : $blank, prefix : $prefix, contenu : $contenu \n";
        	if ( my ($firstelement, $secondelement) = m!.*\(\s*(.*),\s*(.*)\);\s*$!) {
            	my $argument = $prefix;
				# Remove space before argument
				$secondelement =~ s/ //g;
            	$_ = $blank . $secondelement . ".split( " . $firstelement . ");\n" ;
        	}
    	}
    } $file;

}
functionUtilkde::diffFile( "@ARGV" );

