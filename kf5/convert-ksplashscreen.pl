#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KSplashScreen => QSplashScreen
# find -iname "*.cpp" -o -iname "*.h" |xargs kde-dev-scripts/kf5/convert-ksplashscreen.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s/\bKSplashScreen\b/QSplashScreen/g;
        s/\<KSplashScreen\b\>/\<QSplashScreen>/ =~ /#include/ ;
        s/\<ksplashscreen.h\>/\<QSplashScreen>/ =~ /#include/ ;

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
