#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KTabWidget => QTabWidget
# find -iname "*.cpp" -o -iname "*.h" -o -iname "*.ui" |xargs kde-dev-scripts/kf5/convert-ktabwidget.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s/\bKTabWidget\b/QTabWidget/g;
        s/\<KTabWidget\b\>/\<QTabWiget>/ =~ /#include/ ;
        s/\<ktabwidget.h\>/\<QTabWidget>/ =~ /#include/ ;

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
    }
}

functionUtilkde::diffFile( "@ARGV" );
