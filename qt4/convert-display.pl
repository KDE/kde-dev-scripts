#!/usr/bin/perl 

# Laurent Montel <montel@kde.org>
# This function convert qt_xdisplay and qt_rootwin to qt4 function
# it added include too

use lib qw( . );
use functionUtilkde; 

foreach my $file (@ARGV) {
	functionUtilkde::substInFile {
	s!qt_xdisplay\s*\(\s*\)!QX11Info::display()!;
	s!qt_xrootwin\s*\(\s*\)!QX11Info::appRootWindow()!;
    } $file;
   local *F;
   open F, "+<", $file or do { print STDOUT "open($file) failled : \"$!\"\n"; next };
   my $str = join '', <F>;
   $str =~ s!(#include <.*#include <[^
]*)!\1\n#include <QX11Info>!smig;
    seek F, 0, 0;
    print F $str;
    truncate F, tell(F);
}
functionUtilkde::diffFile( "@ARGV" );
