#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# Convert KAboutData to K4AboutData to help us to compile
# find -iname "*.cpp"|xargs kde-dev-scripts/kf5/convert-to-k4aboutdata.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s/\bKAboutData\b/K4AboutData/g;
        s/\<KAboutData\b\>/\<K4AboutData>/ =~ /#include/ ;
        s/\<kaboutdata.h\>/\<K4AboutData>/ =~ /#include/ ;


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
