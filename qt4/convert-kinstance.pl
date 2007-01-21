#!/usr/bin/perl
# laurent Montel <montel@kde.org>
# Hamish Rodda <rodda@kde.org>
# Matthias Kretz <kretz@kde.org>
# Convert KInstance -> KComponentData

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

open(my $F, q(find -name "*" |));
my $file;

while ($file = <$F>) {
    chomp $file;
    next if functionUtilkde::excludeFile( $file);

    my $nbLoop = 1;
    my $modified;
    open(my $FILE, $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        s!#include <kinstance.h>!#include <kcomponentdata.h>!;
        s!#include "kinstance.h"!#include "kcomponentdata.h"!;
        s!kapp->instanceName\s*\(!KGlobal::mainComponent().componentName\(!g;
        s!KInstance::caption\s*\(!KGlobal::caption\(!g;
        s!const\s*KInstance\s*\*\s*!const KComponentData &!g;
        s!KInstance\s*\*\s*!KComponentData !g;
        s!KConfig\s*\*\s*([a-zA-Z0-9_])\s*=\s*KGlobal::config\s*\(!KSharedConfig::Ptr \1 = KGlobal::config\(!g;
        s!KConfig\s*\*\s*([a-zA-Z0-9_])\s*=\s*KGlobal::instance\s*()->config\s*\(!KSharedConfig::Ptr \1 = KGlobal::config\(!g;
        s!KGlobal::instance\s*\(\)->aboutData\s*\(!KGlobal::aboutData\(!g;
        s!KGlobal::instance\s*\(!KGlobal::mainComponent\(!g;
        s!KGlobal::activeInstance\s*\(!KGlobal::activeComponent\(!g;
        s!KGlobal::sharedConfig\s*\(!KGlobal::config\(!g;
        s!->sharedConfig\s*\(!.config\(!g;
        s!->instanceName\s*\(!.componentName\(!g;
        s!\s*=\s*new KInstance\s*\(!\(!g;
        s!new KInstance\s*\(!KComponentData\(!g;
        s!KInstance!KComponentData!g;
        # the following might be a little too bold:
        s!\binstance\b!componentData!g;
        s!\b_instance\b!_componentData!g;
        s!\bm_instance\b!m_componentData!g;
        s!\bmInstance\b!mComponentData!g;

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">$file");
        print $OUT @l;
    }
}
functionUtilkde::diffFile( "@ARGV" );
