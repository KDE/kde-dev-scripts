#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# Adapt new akonadi includes
# find -iname "*.cpp"|xargs kde-dev-scripts/kf5/adapt_knewstuff3_includes.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s,#include \<knewstuff3\/uploaddialog.h\>,#include \<kns3\/uploaddialog.h\>,;
        s,#include \<knewstuff3\/downloaddialog.h\>,#include \<kns3\/downloaddialog.h\>,;
        s,#include \<knewstuff3\/downloadmanager.h\>,#include \<kns3\/downloadmanager.h\>,;
        s,#include \<knewstuff3\/downloadwidget.h\>,#include \<kns3\/downloadwidget.h\>,;
        s,#include \<knewstuff3\/entry.h\>,#include \<kns3\/entry.h\>,;
        s,#include \<knewstuff3\/knewstuffaction.h\>,#include \<kns3\/knewstuffaction.h\>,;
        s,#include \<knewstuff3\/knewstuffbutton.h\>,#include \<kns3\/knewstuffbutton.h\>,;

        
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
