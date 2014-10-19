#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KTextBrowser => QTextBrowser
# find -iname "*.cpp" -o -iname "*.h" -o -iname "*.ui" |xargs kde-dev-scripts/kf5/convert-ktextbrowser.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

#TODO :  * - setNotifyClick becomes setOpenLinks, isNotifyClick is set to openLinks.
# * - use the signal QTextBrowser::anchorClicked for KTextBrowser::urlClick

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s/\bKTextBrowser\b/QTextBrowser/g;
        s/\<KTextBrowser\b\>/\<QTextBrowser>/ if (/#include/);
        s/\<ktextbrowser.h\>/\<QTextBrowser>/ if (/#include/);
        
        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        warn "QTextBrowser does not support \"whatsthis:\" urls. \n";
    }
}

functionUtilkde::diffFile( "@ARGV" );
