#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# Adapt new akonadi includes
# find -iname "*.cpp"|xargs kde-dev-scripts/kf5/adapt-akonadi-includes.pl

# grep -r "#include " |grep akonadi |cut -d: -f2 |sort |uniq
 
use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        s,#include \<akonadi\/item.h\>,#include \<AkonadiCore\/item.h\>,;
        s,#include \<akonadi\/collection.h\>,#include \<AkonadiCore\/collection.h\>,;
        s,#include \<akonadi\/attribute.h\>,#include \<AkonadiCore\/attribute.h\>,;
        s,#include \<akonadi\/attributefactory.h\>,#include \<AkonadiCore\/attributefactory.h\>,;
        s,#include \<akonadi\/entitytreemodel.h\>,#include \<AkonadiCore\/entitytreemodel.h\>,;
        s,#include \<akonadi\/agentmanager.h\>,#include \<AkonadiCore\/agentmanager.h\>,;

        s,#include \<akonadi\/agentinstancecreatejob.h\>,#include \<AkonadiCore\/agentinstancecreatejob.h\>,;
        s,#include \<akonadi\/agentinstance.h\>,#include \<AkonadiCore\/agentinstance.h\>,;
        s,#include \<akonadi\/changerecorder.h\>,#include \<AkonadiCore\/changerecorder.h\>,;
        s,#include \<akonadi\/collectiondialog.h\>,#include \<AkonadiWidgets\/collectiondialog.h\>,;
        s,#include \<akonadi\/collectionfetchjob.h\>,#include \<AkonadiCore\/collectionfetchjob.h\>,;
        s,#include \<akonadi\/collectionfetchscope.h\>,#include \<AkonadiCore\/collectionfetchscope.h\>,;
        s,#include \<akonadi\/collectionfilterproxymodel.h\>,#include \<AkonadiCore\/collectionfilterproxymodel.h\>,;
        s,#include \<akonadi\/entitydisplayattribute.h\>,#include \<AkonadiCore\/entitydisplayattribute.h\>,;
        s,#include \<akonadi\/entitymimetypefiltermodel.h\>,#include \<AkonadiCore\/entitymimetypefiltermodel.h\>,;
        s,#include \<akonadi\/itemcreatejob.h\>,#include \<AkonadiCore\/itemcreatejob.h\>,;
        s,#include \<akonadi\/itemdeletejob.h\>,#include \<AkonadiCore\/itemdeletejob.h\>,;
        s,#include \<akonadi\/itemfetchjob.h\>,#include \<AkonadiCore\/itemfetchjob.h\>,;
        s,#include \<akonadi\/itemfetchscope.h\>,#include \<AkonadiCore\/itemfetchscope.h\>,;
        s,#include \<akonadi\/itemmodifyjob.h\>,#include \<AkonadiCore\/itemmodifyjob.h\>,;
        s,#include \<akonadi\/job.h\>,#include \<AkonadiCore\/job.h\>,;
        s,#include \<akonadi\/mimetypechecker.h\>,#include \<AkonadiCore\/mimetypechecker.h\>,;
        s,#include \<akonadi\/session.h\>,#include \<AkonadiCore\/session.h\>,;
        s,#include \<akonadi\/standardactionmanager.h\>,#include \<AkonadiCore\/standardactionmanager.h\>,;
        s,#include \<akonadi\/transactionsequence.h\>,#include \<AkonadiCore\/transactionsequence.h\>,;
        s,#include \<akonadi\/agenttype.h\>,#include \<AkonadiCore\/agenttype.h\>,;
        s,#include \<akonadi\/attributefactory.h\>,#include \<AkonadiCore\/attributefactory.h\>,;
        s,#include \<akonadi\/monitor.h\>,#include \<AkonadiCore\/monitor.h\>,;
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
