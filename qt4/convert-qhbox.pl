#!/usr/bin/perl 
# laurent Montel <montel@kde.org>

# TEMPORARY script:
# David will create a khbox/kvbox to reduce convert time to convert

use lib qw( . );
use functionUtilkde; 

foreach my $file (@ARGV) {
    my $nbLoop = 1;
    functionUtilkde::substInFile {
	if (my ($blank, $prefix, $contenu) = m!^(\s*)(.*)new Q3HBox\s*\(\s*([^,]*)\);\s*$!) {
	    warn "increate $nbLoop \n";
	    my $initial = $prefix;
	    my $prefixinitial = $prefix;
	    $prefixinitial =~ s!Q3HBox!QWidget!;
	    $initial =~ s!Q3HBox\s*\*!!;
	    $initial =~ s!=(.*)!!;
#remove all space and tab
	    $initial =~ tr/ \t//d;
	    my $hboxlayoutname = "hboxLayout" . $nbLoop;
	    $_ = $blank . $prefixinitial . "new QWidget($contenu);\n" . $blank . "QHBoxLayout *$hboxlayoutname = new QHBoxLayout($initial);\n" . $blank . $initial ."->setLayout($hboxlayoutname);\n";
	    $nbLoop = $nbLoop+1;
	}

	if (my ($blank, $prefix, $contenu) = m!^(\s*)(.*)new Q3VBox\s*\(\s*([^,]*)\);\s*$!) {
	    warn "increate $nbLoop \n";
	    my $initial = $prefix;
	    my $prefixinitial = $prefix;
	    $prefixinitial =~ s!Q3VBox!QWidget!;
	    $initial =~ s!Q3VBox\s*\*!!;
	    $initial =~ s!QWidget\s*\*!!;
	    $initial =~ s!=(.*)!!;
#remove all space and tab
	    $initial =~ tr/ \t//d;
	    my $vboxlayoutname = "vboxLayout" . $nbLoop;
	    $_ = $blank . $prefixinitial . "new QWidget($contenu);\n" . $blank . "QVBoxLayout *$vboxlayoutname = new QVBoxLayout($initial);\n" . $blank . $initial ."->setLayout($vboxlayoutname);\n";
	    $nbLoop = $nbLoop+1;
	}

	s!^(\s*)(.*)new Q3HBox\s*;!\1\2new QWidget;\n\1QHBoxLayout *hboxLayout = new QHBoxLayout(hbox);\n\1hbox->setLayout(hboxLayout);!;
	


	s!#include <q3vbox.h>!!;
    } $file;

}
functionUtilkde::diffFile( "@ARGV" );
