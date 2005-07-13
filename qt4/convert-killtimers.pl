#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function convert killTimers to qt4 function

use lib qw( . );
use functionUtilkde; #utilisation des fonctions kde qui me sont utiles


foreach my $file (@ARGV) {
    #TODO fix for killTimers with arguments
    functionUtilkde::substInFile {
	s!killTimers\s*\(\s*\);!QAbstractEventDispatcher::instance()->unregisterTimers(this);!;
    } $file;
   local *F;
   open F, "+<", $file or do { print STDOUT "open($file) failled : \"$!\"\n"; next };
   my $str = join '', <F>;
   $str =~ s!(#include <.*#include <[^
]*)!\1\n#include <QAbstractEventDispatcher>!smig;
    seek F, 0, 0;
    print F $str;
    truncate F, tell(F);
}
functionUtilkde::diffFile( "@ARGV" );

