#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KSharedPtr->QExplicitlySharedDataPointer
# find -iname "*.cpp" -o -iname "*.h" -o -iname "*.ui" |xargs kde-dev-scripts/kf5/convert-ksharedptr.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s/\bKSharedPtr\b/QExplicitlySharedDataPointer/g;
        s/\bKShared\b/QSharedData/g;
        s/\<KSharedPtr\b\>/\<QExplicitlySharedDataPointer>/ =~ /#include/ ;
        s/\<ksharedptr.h\>/\<QExplicitlySharedDataPointer>/ =~ /#include/ ;

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
