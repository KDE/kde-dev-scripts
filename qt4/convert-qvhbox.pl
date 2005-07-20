#!/usr/bin/perl 
# laurent Montel <montel@kde.org>
# convert q3hvbox->kh/vbox

use lib qw( . );
use functionUtilkde; 

foreach my $file (@ARGV) {
    my $nbLoop = 1;
    functionUtilkde::substInFile {
		s!Q3VBox!KVBox!g;
		s!Q3HBox!KHBox!g;
	s!#include <q3vbox.h>!!;
	s!#include <q3hbox.h>!!;
    } $file;
   local *F;
   open F, "+<", $file or do { print STDOUT "open($file) failled : \"$!\"\n"; next };
   my $str = join '', <F>;
   $str =~ s!(#include <.*#include <[^
]*)!\1\n#include <kvbox.h>!smig;
    seek F, 0, 0;
    print F $str;
    truncate F, tell(F);
	close F;
}
functionUtilkde::diffFile( "@ARGV" );
