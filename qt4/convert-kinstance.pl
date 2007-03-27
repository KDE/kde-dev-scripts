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

    my $modified;
    my @necessaryIncludes = ();
    open(my $FILE, $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        s!#include <kinstance.h>!#include <kcomponentdata.h>!;
        s!#include "kinstance.h"!#include "kcomponentdata.h"!;
        s!kapp->instanceName\s?\(!KGlobal::mainComponent().componentName(!g;
        s!kapp->dirs\s?\(!KGlobal::dirs(!g;
        s!KApplication::kApplication\s?\(\)->dirs\s?\(!KGlobal::dirs(!g;
        s!kapp->aboutData\s?\(!KGlobal::mainComponent().aboutData(!g;
        s!kapp->caption\s?\(!KGlobal::caption(!g;
        s!KInstance::caption\s?\(!KGlobal::caption(!g;
        s!const\s*KInstance\s*\*\s*!const KComponentData &!g;
        s!KInstance\s*\*\s*!KComponentData !g;
        s!KConfig\s*\*\s*([a-zA-Z0-9_]+)\s*=\s*kapp->config\s?\(!KSharedConfig::Ptr \1 = KGlobal::config(!g;
        s!KConfig\s*\*\s*([a-zA-Z0-9_]+)\s*=\s*KGlobal::config\s?\(!KSharedConfig::Ptr \1 = KGlobal::config(!g;
        s!KConfig\s*\*\s*([a-zA-Z0-9_]+)\s*=\s*KGlobal::instance\s?()->config\s*\(!KSharedConfig::Ptr \1 = KGlobal::config(!g;
        s!KGlobal::instance\s?\(\)->!KGlobal::mainComponent().!g;
        s!KGlobal::instance\s?\(!KGlobal::mainComponent(!g;
        s!KGlobal::activeInstance\s?\(!KGlobal::activeComponent(!g;
        s!KGlobal::setActiveInstance\s?\(!KGlobal::setActiveComponent(!g;
        s!KGlobal::sharedConfig\s?\(!KGlobal::config(!g;
        s!->sharedConfig\s?\(!.config(!g;
        s!->instanceName\s?\(!.componentName(!g;
        s!\s*=\s*new KInstance\s*\(!(!g;
        s!new KInstance\s?\(!KComponentData(!g;
        s!KInstance!KComponentData!g;
        # the following might be a little too bold:
        s!KConfig\s*\*\s*([a-zA-Z0-9_]+)\s*=\s*instance->config\s?\(!KSharedConfig::Ptr \1 = componentData.config(!g;
        s!KConfig\s*\*\s*([a-zA-Z0-9_]+)\s*=\s*instance\(\)->config\s?\(!KSharedConfig::Ptr \1 = componentData().config(!g;
        s!KConfig\s*\*\s*([a-zA-Z0-9_]+)\s*=\s*_instance->config\s?\(!KSharedConfig::Ptr \1 = _componentData.config(!g;
        s!KConfig\s*\*\s*([a-zA-Z0-9_]+)\s*=\s*m_instance->config\s?\(!KSharedConfig::Ptr \1 = m_componentData.config(!g;
        s!KConfig\s*\*\s*([a-zA-Z0-9_]+)\s*=\s*mInstance->config\s?\(!KSharedConfig::Ptr \1 = mComponentData.config(!g;
        s!instanceNames\s?\(!componentNames(!g;
        s!\binstance->!componentData.!g;
        s!\b_instance->!_componentData.!g;
        s!\bm_instance->!m_componentData.!g;
        s!\bmInstance->!mComponentData.!g;
        s!\binstance\s?\(\)->!componentData().!g;
        s!\binstance\s?\(!componentData(!g;
        s!\bpartInstance\s?\(!partComponentData(!g;
        s!\bpartInstanceFromLibrary\s?\(!partComponentDataFromLibrary(!g;
        s!\bcreateInstance\s?\(!createComponentData(!g;
        #s!\binstance\b!componentData!g;
        s!\b_instance\b!_componentData!g;
        s!\bm_instance\b!m_componentData!g;
        s!\bmInstance\b!mComponentData!g;
        s!\bsetInstance\b!setComponentData!g;
        s!\binstanceName\b!componentName!g;
        # wrong changes:
        s!QAbstractEventDispatcher::componentData\s?\(\)\.!QAbstractEventDispatcher::instance()->!g;
        s!QAbstractEventDispatcher::componentData\s?\(!QAbstractEventDispatcher::instance(!g;
        s!QCoreApplication::componentData\s?\(\)\.!QCoreApplication::instance()->!g;
        s!QCoreApplication::componentData\s?\(!QCoreApplication::instance(!g;
        s!QApplication::componentData\s?\(\)\.!QApplication::instance()->!g;
        s!QApplication::componentData\s?\(!QApplication::instance(!g;

        if ($_ =~ /KConfigGroup/) {
                push(@necessaryIncludes, "kconfiggroup.h");
        }

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">$file");
        print $OUT @l;
    }

    my %alreadyadded = {};
    foreach my $inc (@necessaryIncludes) {
        next if (defined $alreadyadded{$inc});
        $alreadyadded{$inc} = 1;

        functionUtilkde::addIncludeInFile($file, $inc);
    }
}
functionUtilkde::diffFile( "@ARGV" );
# vim: et sw=4
