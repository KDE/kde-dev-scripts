#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KPushButton => QPushButton
# find -iname "*.cpp" -o -iname "*.h" -o -iname "*.ui" |xargs kde-dev-scripts/kf5/convert-kpushbutton.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s/\bKPushButton\b/QPushButton/g;
        s/\<KPushButton\b\>/\<QPushButton>/ =~ /#include/ ;
        s/\<kpushbutton.h\>/\<QPushButton>/ =~ /#include/ ;

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
