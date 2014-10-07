#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KFontComboBox => QFontComboBox
# find -iname "*.cpp" -o -iname "*.h" -o -iname "*.ui" |xargs kde-dev-scripts/kf5/convert-kfontcombobox.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s/\bKFontComboBox\b/QFontComboBox/g;
        s/\<KFontComboBox\b\>/\<QFontComboBox>/ if (/#include/);
        s/\<kfontcombobox.h\>/\<QFontComboBox>/ if (/#include/);
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
