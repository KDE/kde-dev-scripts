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
        #s,#include \<[Aa]konadi\/\>,#include \<AkonadiCore\/\>,;
        s,#include \<[Aa]konadi\/CollectionDeleteJob\>,#include \<AkonadiCore\/CollectionDeleteJob\>,;
        s,#include \<[Aa]konadi\/SearchCreateJob\>,#include \<AkonadiCore\/SearchCreateJob\>,;
        s,#include \<[Aa]konadi\/persistentsearchattribute.h\>,#include \<AkonadiCore\/persistentsearchattribute.h\>,;
        s,#include \<[Aa]konadi\/CollectionModifyJob\>,#include \<AkonadiCore\/CollectionModifyJob\>,;
        s,#include \<[Aa]konadi\/AgentInstanceModel\>,#include \<AkonadiCore\/AgentInstanceModel\>,;
        s,#include \<[Aa]konadi\/collectionquotaattribute.h\>,#include \<AkonadiCore\/collectionquotaattribute.h\>,;
        s,#include \<[Aa]konadi\/indexpolicyattribute.h\>,#include \<AkonadiCore\/indexpolicyattribute.h\>,;
        s,#include \<[Aa]konadi\/ItemMoveJob\>,#include \<AkonadiCore\/ItemMoveJob\>,;
        s,#include \<[Aa]konadi\/CollectionCreateJob\>,#include \<AkonadiCore\/CollectionCreateJob\>,;
        s,#include \<[Aa]konadi\/TagAttribute\>,#include \<AkonadiCore\/TagAttribute\>,;
        s,#include \<[Aa]konadi\/TagFetchJob\>,#include \<AkonadiCore\/TagFetchJob\>,;
        s,#include \<[Aa]konadi\/TagFetchScope\>,#include \<AkonadiCore\/TagFetchScope\>,;
        s,#include \<[Aa]konadi\/Monitor\>,#include \<AkonadiCore\/Monitor\>,;
        s,#include \<[Aa]konadi\/CachePolicy\>,#include \<AkonadiCore\/CachePolicy\>,;
        s,#include \<[Aa]konadi\/collectiondeletejob.h\>,#include \<AkonadiCore\/collectiondeletejob.h\>,;
        s,#include \<[Aa]konadi\/collectionstatisticsdelegate.h\>,#include \<AkonadiWidgets\/collectionstatisticsdelegate.h\>,;
        s,#include \<[Aa]konadi\/agentinstancemodel.h\>,#include \<AkonadiCore\/agentinstancemodel.h\>,;
        s,#include \<[Aa]konadi\/collectionattributessynchronizationjob.h\>,#include \<AkonadiCore\/collectionattributessynchronizationjob.h\>,;
        s,#include \<[Aa]konadi\/collectionstatistics.h\>,#include \<AkonadiCore\/collectionstatistics.h\>,;
        s,#include \<[Aa]konadi\/Entity\>,#include \<AkonadiCore\/Entity\>,;
        s,#include \<[Aa]konadi\/AgentInstanceCreateJob\>,#include \<AkonadiCore\/AgentInstanceCreateJob\>,;
        s,#include \<[Aa]konadi\/CollectionStatisticsJob\>,#include \<AkonadiCore\/CollectionStatisticsJob\>,;
        s,#include \<[Aa]konadi\/itemsearchjob.h\>,#include \<AkonadiCore\/itemsearchjob.h\>,;
        s,#include \<[Aa]konadi\/itemcopyjob.h\>,#include \<AkonadiCore\/itemcopyjob.h\>,;
        s,#include \<[Aa]konadi\/tagmodel.h\>,#include \<AkonadiCore\/tagmodel.h\>,;
        s,#include \<[Aa]konadi\/searchcreatejob.h\>,#include \<AkonadiCore\/searchcreatejob.h\>,;
        s,#include \<[Aa]konadi\/favoritecollectionsmodel.h\>,#include \<AkonadiCore\/favoritecollectionsmodel.h\>,;
        s,#include \<[Aa]konadi\/ServerManager\>,#include \<AkonadiCore\/ServerManager\>,;
        s,#include \<[Aa]konadi\/selectionproxymodel.h\>,#include \<AkonadiCore\/selectionproxymodel.h\>,;
        s,#include \<[Aa]konadi\/collectionmodifyjob.h\>,#include \<AkonadiCore\/collectionmodifyjob.h\>,;
        s,#include \<[Aa]konadi\/servermanager.h\>,#include \<AkonadiCore\/servermanager.h\>,;
        s,#include \<[Aa]konadi\/AgentFilterProxyModel\>,#include \<AkonadiCore\/AgentFilterProxyModel\>,;
        s,#include \<[Aa]konadi\/agentfilterproxymodel.h\>,#include \<AkonadiCore\/agentfilterproxymodel.h\>,;
        s,#include \<[Aa]konadi\/agentinstance.h\>,#include \<AkonadiCore\/agentinstance.h\>,;
        s,#include \<[Aa]konadi\/AgentInstance\>,#include \<AkonadiCore\/AgentInstance\>,;
        s,#include \<Akonadi\/dbusconnectionpool.h\>,#include \<AkonadiCore\/dbusconnectionpool.h\>,;
        s,#include \<akonadi\/dbusconnectionpool.h\>,#include \<AkonadiCore\/dbusconnectionpool.h\>,;
        s,#include \<akonadi\/itemmovejob.h\>,#include \<AkonadiCore\/itemmovejob.h\>,;
        s,#include \<Akonadi\/CollectionFetchScope\>,#include \<AkonadiCore\/CollectionFetchScope\>,;
        s,#include \<Akonadi\/AgentManager\>,#include \<AkonadiCore\/AgentManager\>,;
        s,#include \<akonadi\/control.h\>,#include \<AkonadiCore\/control.h\>,;
        s,#include \<akonadi\/entityannotationsattribute.h\>,#include \<AkonadiCore\/entityannotationsattribute.h\>,;
        s,#include \<akonadi\/searchquery.h\>,#include \<AkonadiCore\/searchquery.h\>,;
        s,#include \<akonadi\/tagcreatejob.h\>,#include \<AkonadiCore\/tagcreatejob.h\>,;
        s,#include \<Akonadi\/ItemFetchJob\>,#include \<AkonadiCore\/ItemFetchJob\>,;
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
        s,#include \<akonadi\/tag.h\>,#include \<AkonadiCore\/tag.h\>,;
        #AkonadiWidgets
        s,#include \<Akonadi/CollectionComboBox\>,#include \<AkonadiWidgets\/CollectionComboBox\>,;
	s,#include \<Akonadi\/agentactionmanager.h\>,#include \<AkonadiWidgets\/agentactionmanager.h\>,;                                                                                                                                                                                                                                                           
        s,#include \<Akonadi\/AgentInstanceWidget\>,#include \<AkonadiWidgets\/ agentactionmanager.h\>,;                                                                                                                                                                                                                                                           
        s,#include \<[Aa]konadi\/agentinstancewidget.h\>,#include \<AkonadiWidgets\/agentinstancewidget.h\>,;                                                                                                                                                                                                                                                          
        s,#include \<Akonadi\/AgentTypeDialog\>,#include \<AkonadiWidgets\/AgentTypeDialog\>,;                                                                                                                                                                                                                                                               
        s,#include \<[Aa]konadi\/agenttypedialog.h\>,#include \<AkonadiWidgets\/agenttypedialog.h\>,;                                                                                                                                                                                                                                                             
        s,#include \<Akonadi\/AgentTypeWidget\>,#include \<AkonadiWidgets\/AgentTypeWidget\>,;                                                                                                                                                                                                                                                               
        s,#include \<Akonadi\/agenttypewidget.h\>,#include \<AkonadiWidgets\/agenttypewidget.h\>,;                                                                                                                                                                                                                                                              
        s,#include \<Akonadi\/collectioncombobox.h\>,#include \<AkonadiWidgets\/collectioncombobox.h\>,;
        s,#include \<akonadi\/collectioncombobox.h\>,#include \<AkonadiWidgets\/collectioncombobox.h\>,;
        s,#include \<Akonadi\/CollectionDialog\>,#include \<AkonadiWidgets\/CollectionDialog\>,;
        s,#include \<Akonadi\/collectiondialog.h\>,#include \<AkonadiWidgets\/collectiondialog.h\>,;
        s,#include \<[Aa]konadi\/CollectionPropertiesDialog\>,#include \<AkonadiWidgets\/CollectionPropertiesDialog\>,;
        s,#include \<[Aa]konadi\/collectionpropertiesdialog.h\>,#include \<AkonadiWidgets\/collectionpropertiesdialog.h\>,;
        s,#include \<Akonadi\/collectionpropertiesdialog.h\>,#include \<AkonadiWidgets\/collectionpropertiesdialog.h\>,;
        s,#include \<Akonadi\/CollectionPropertiesPage\>,#include \<AkonadiWidgets\/CollectionPropertiesPage\>,;
        s,#include \<akonadi\/collectionpropertiespage.h\>,#include \<AkonadiWidgets\/collectionpropertiespage.h\>,;
        s,#include \<Akonadi\/collectionpropertiespage.h\>,#include \<AkonadiWidgets\/collectionpropertiespage.h\>,;
        s,#include \<Akonadi\/CollectionRequester\>,#include \<AkonadiWidgets\/CollectionRequester\>,;
        s,#include \<Akonadi\/collectionrequester.h\>,#include \<AkonadiWidgets\/collectionrequester.h\>,;
        s,#include \<akonadi\/collectionrequester.h\>,#include \<AkonadiWidgets\/collectionrequester.h\>,;
        s,#include \<Akonadi\/CollectionStatisticsDelegate\>,#include \<AkonadiWidgets\/CollectionStatisticsDelegate\>,;
        s,#include \<Akonadi\/collectionstatisticsdelegate.h\>,#include \<AkonadiWidgets\/collectionstatisticsdelegate.h\>,;
        s,#include \<Akonadi\/CollectionView\>,#include \<AkonadiWidgets\/CollectionView\>,;
        s,#include \<Akonadi\/collectionview.h\>,#include \<AkonadiWidgets\/collectionview.h\>,;
        s,#include \<Akonadi\/EntityListView\>,#include \<AkonadiWidgets\/EntityListView\>,;
        s,#include \<[Aa]konadi\/entitylistview.h\>,#include \<AkonadiWidgets\/entitylistview.h\>,;
        s,#include \<Akonadi\/EntityTreeView\>,#include \<AkonadiWidgets\/EntityTreeView\>,;
        s,#include \<[Aa]konadi\/entitytreeview.h\>,#include \<AkonadiWidgets\/entitytreeview.h\>,;
        s,#include \<Akonadi\/EntityTreeViewStateSaver\>,#include \<AkonadiWidgets\/EntityTreeViewStateSaver\>,;
        s,#include \<Akonadi\/entitytreeviewstatesaver.h\>,#include \<AkonadiWidgets\/entitytreeviewstatesaver.h\>,;
        s,#include \<Akonadi\/ETMViewStateSaver\>,#include \<AkonadiWidgets\/ETMViewStateSaver\>,;
        s,#include \<[Aa]konadi\/etmviewstatesaver.h\>,#include \<AkonadiWidgets\/etmviewstatesaver.h\>,;
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
        
        #s,#include \<Akonadi\/\>,#include \<AkonadiAgentBase\/\>,;
        s,#include \<Akonadi\/agentbase.h\>,#include \<AkonadiAgentBase\/agentbase.h\>,;
        s,#include \<akonadi\/agentbase.h\>,#include \<AkonadiAgentBase\/agentbase.h\>,;
        
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
