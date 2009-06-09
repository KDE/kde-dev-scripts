#!/usr/bin/perl 
# laurent Montel <montel@kde.org>
# convert q3hvbox->kh/vbox

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

open(my $F, q(find -name "*" |));
my $file;

while ($file = <$F>) {
    chomp $file;
    next if functionUtilkde::excludeFile( $file);

    my $modified;
    open(my $FILE, $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
	s!#include <q3vbox.h>!!;
	s!#include <q3hbox.h>!!;
	s!#include <Q3VBox>!!;
	s!#include <Qt3Support/Q3VBox>!!;
	s!#include <Q3HBox>!!;
	s!#include <Qt3Support/Q3HBox>!!;
	s!\bQ3VBox\b!KVBox!g;
	s!\bQ3HBox\b!KHBox!g;
    
        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">$file");
        print $OUT @l;
        close ($OUT);
        functionUtilkde::addIncludeInFile( $file, "kvbox.h");
    }

}
functionUtilkde::diffFile( "@ARGV" );
