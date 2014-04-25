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
        s,#include \<Akonadi\/EntityTreeModel\>,#include \<AkonadiCore\/EntityTreeModel\>,;
        s,#include \<Akonadi\/ChangeRecorder\>,#include \<AkonadiCore\/ChangeRecorder\>,;
        s,#include \<Akonadi\/EntityDisplayAttribute\>,#include \<AkonadiCore\/EntityDisplayAttribute\>,;
        s,#include \<Akonadi\/ItemDeleteJob\>,#include \<AkonadiCore\/ItemDeleteJob\>,;
        #s,#include \<Akonadi\/\>,#include \<AkonadiCore\/\>,;
        s,#include \<Akonadi\/CollectionFetchJob\>,#include \<AkonadiCore\/CollectionFetchJob\>,;
        s,#include \<Akonadi\/ItemCreateJob\>,#include \<AkonadiCore\/ItemCreateJob\>,;
        s,#include \<Akonadi\/AttributeFactory\>,#include \<AkonadiCore\/AttributeFactory\>,;
        s,#include \<Akonadi\/MimeTypeChecker\>,#include \<AkonadiCore\/MimeTypeChecker\>,;
        s,#include \<Akonadi\/EntityMimeTypeFilterModel\>,#include \<AkonadiCore\/EntityMimeTypeFilterModel\>,;
        s,#include \<Akonadi\/Control\>,#include \<AkonadiCore\/Control\>,;
        s,#include \<Akonadi\/CollectionModel\>,#include \<AkonadiCore\/CollectionModel\>,;
        s,#include \<Akonadi\/CollectionFilterProxyModel\>,#include \<AkonadiCore\/CollectionFilterProxyModel\>,;
        s,#include \<Akonadi\/ItemFetchScope\>,#include \<AkonadiCore\/ItemFetchScope\>,;
        s,#include \<Akonadi\/Session\>,#include \<AkonadiCore\/Session\>,;
        s,#include \<Akonadi\/ItemModifyJob\>,#include \<AkonadiCore\/ItemModifyJob\>,;
        #AkonadiWidgets
        s,#include \<Akonadi/CollectionComboBox\>,#include \<AkonadiWidgets\/CollectionComboBox\>,;
	s,#include \<Akonadi\/agentactionmanager.h\>,#include \<AkonadiWidgets\/agentactionmanager.h\>,;                                                                                                                                                                                                                                                           
        s,#include \<Akonadi\/AgentInstanceWidget\>,#include \<AkonadiWidgets\/ agentactionmanager.h\>,;                                                                                                                                                                                                                                                           
        s,#include \<Akonadi\/agentinstancewidget.h\>,#include \<AkonadiWidgets\/agentinstancewidget.h\>,;                                                                                                                                                                                                                                                          
        s,#include \<Akonadi\/AgentTypeDialog\>,#include \<AkonadiWidgets\/AgentTypeDialog\>,;                                                                                                                                                                                                                                                               
        s,#include \<Akonadi\/agenttypedialog.h\>,#include \<AkonadiWidgets\/agenttypedialog.h\>,;                                                                                                                                                                                                                                                             
        s,#include \<Akonadi\/AgentTypeWidget\>,#include \<AkonadiWidgets\/AgentTypeWidget\>,;                                                                                                                                                                                                                                                               
        s,#include \<Akonadi\/agenttypewidget.h\>,#include \<AkonadiWidgets\/agenttypewidget.h\>,;                                                                                                                                                                                                                                                              
        s,#include \<Akonadi\/collectioncombobox.h\>,#include \<AkonadiWidgets\/collectioncombobox.h\>,;
        s,#include \<Akonadi\/CollectionDialog\>,#include \<AkonadiWidgets\/CollectionDialog\>,;
        s,#include \<Akonadi\/collectiondialog.h\>,#include \<AkonadiWidgets\/collectiondialog.h\>,;
        s,#include \<Akonadi\/CollectionPropertiesDialog\>,#include \<AkonadiWidgets\/CollectionPropertiesDialog\>,;
        s,#include \<Akonadi\/collectionpropertiesdialog.h\>,#include \<AkonadiWidgets\/collectionpropertiesdialog.h\>,;
        s,#include \<Akonadi\/CollectionPropertiesPage\>,#include \<AkonadiWidgets\/CollectionPropertiesPage\>,;
        s,#include \<Akonadi\/collectionpropertiespage.h\>,#include \<AkonadiWidgets\/collectionpropertiespage.h\>,;
        s,#include \<Akonadi\/CollectionRequester\>,#include \<AkonadiWidgets\/CollectionRequester\>,;
        s,#include \<Akonadi\/collectionrequester.h\>,#include \<AkonadiWidgets\/collectionrequester.h\>,;
        s,#include \<Akonadi\/CollectionStatisticsDelegate\>,#include \<AkonadiWidgets\/CollectionStatisticsDelegate\>,;
        s,#include \<Akonadi\/collectionstatisticsdelegate.h\>,#include \<AkonadiWidgets\/collectionstatisticsdelegate.h\>,;
        s,#include \<Akonadi\/CollectionView\>,#include \<AkonadiWidgets\/CollectionView\>,;
        s,#include \<Akonadi\/collectionview.h\>,#include \<AkonadiWidgets\/collectionview.h\>,;
        s,#include \<Akonadi\/EntityListView\>,#include \<AkonadiWidgets\/EntityListView\>,;
        s,#include \<Akonadi\/entitylistview.h\>,#include \<AkonadiWidgets\/entitylistview.h\>,;
        s,#include \<Akonadi\/EntityTreeView\>,#include \<AkonadiWidgets\/EntityTreeView\>,;
        s,#include \<Akonadi\/entitytreeview.h\>,#include \<AkonadiWidgets\/entitytreeview.h\>,;
        s,#include \<Akonadi\/EntityTreeViewStateSaver\>,#include \<AkonadiWidgets\/EntityTreeViewStateSaver\>,;
        s,#include \<Akonadi\/entitytreeviewstatesaver.h\>,#include \<AkonadiWidgets\/entitytreeviewstatesaver.h\>,;
        s,#include \<Akonadi\/ETMViewStateSaver\>,#include \<AkonadiWidgets\/ETMViewStateSaver\>,;
        s,#include \<Akonadi\/etmviewstatesaver.h\>,#include \<AkonadiWidgets\/etmviewstatesaver.h\>,;
        s,#include \<Akonadi\/ItemView\>,#include \<AkonadiWidgets\/ItemView\>,;
        s,#include \<Akonadi\/itemview.h\>,#include \<AkonadiWidgets\/itemview.h\>,;
        s,#include \<Akonadi\/RenameFavoriteDialog\>,#include \<AkonadiWidgets\/RenameFavoriteDialog\>,;
        s,#include \<Akonadi\/renamefavoritedialog.h\>,#include \<AkonadiWidgets\/renamefavoritedialog.h\>,;
        s,#include \<Akonadi\/StandardActionManager\>,#include \<AkonadiWidgets\/StandardActionManager\>,;
        s,#include \<Akonadi\/standardactionmanager.h\>,#include \<AkonadiWidgets\/standardactionmanager.h\>,;
        s,#include \<Akonadi\/SubscriptionDialog\>,#include \<AkonadiWidgets\/SubscriptionDialog\>,;
        s,#include \<Akonadi\/subscriptiondialog.h\>,#include \<AkonadiWidgets\/subscriptiondialog.h\>,;
        s,#include \<Akonadi\/TagManagementDialog\>,#include \<AkonadiWidgets\/TagManagementDialog\>,;
        s,#include \<Akonadi\/tagmanagementdialog.h\>,#include \<AkonadiWidgets\/tagmanagementdialog.h\>,;
        s,#include \<Akonadi\/TagSelectionDialog\>,#include \<AkonadiWidgets\/TagSelectionDialog\>,;
        s,#include \<Akonadi\/tagselectiondialog.h\>,#include \<AkonadiWidgets\/tagselectiondialog.h\>,;
        s,#include \<Akonadi\/TagWidget\>,#include \<AkonadiWidgets\/TagWidget\>,;
        s,#include \<Akonadi\/tagwidget.h\>,#include \<AkonadiWidgets\/tagwidget.h\>,;
        
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
