#!/usr/bin/perl 
# laurent Montel <montel@kde.org>
# convert q3hvbox->kh/vbox

use lib qw( . );
use functionUtilkde; 

foreach my $file (@ARGV) {
    my $nbLoop = 1;
    functionUtilkde::substInFile {
	s!#include <q3vbox.h>!!;
	s!#include <q3hbox.h>!!;
	s!#include <Q3VBox>!!;
	s!#include <Q3HBox>!!;
		s!Q3VBox!KVBox!g;
		s!Q3HBox!KHBox!g;
    } $file;

    functionUtilkde::addIncludeInFile( $file, "kvbox.h");
}
functionUtilkde::diffFile( "@ARGV" );
