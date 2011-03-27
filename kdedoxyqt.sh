#! /bin/sh
#
# kdedoxyqt.sh
# Copyright 2008,2010 by Allen Winter <winter@kde.org>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#

# A program to generate KDE API dox suitable for Qt Assistant using doxygen.

#check to make sure the version is 1.5.7 or above
V=`doxygen --version | awk -F. '{printf("%d%d%d",$1,$2,$3)}'`
if ( test $V -lt 157 ) then
  echo "Must have doxygen version 1.5.7 or above to use this program. Exiting..."
  exit 1
fi

usage="`basename $0` [-n <project_name>] [-x <project_version>]"

project_name="KDE"
project_version="4.7"
while getopts "hn:x:" options; do
  case $options in
    h ) echo $usage; exit 0;;
    n ) project_name="$OPTARG";;
    x ) project_version="$OPTARG";;
    \? ) echo $usage
         exit 1;;
    * ) echo $usage
          exit 1;;

  esac
done

virtual_folder="$project_name"-"$project_version"

( cat <<EOF ) | doxygen -
#---------------------------------------------------------------------------
# Project related configuration options
#---------------------------------------------------------------------------
PROJECT_NAME           = $project_name
PROJECT_NUMBER         = $project_version
OUTPUT_DIRECTORY       = apidocs
CREATE_SUBDIRS         = NO
OUTPUT_LANGUAGE        = English
BRIEF_MEMBER_DESC      = YES
REPEAT_BRIEF           = YES
ABBREVIATE_BRIEF       = "The \$name class" \
                         "The \$name widget" \
                         "The \$name file" \
                         is \
                         provides \
                         specifies \
                         contains \
                         represents \
                         a \
                         an \
                         the
ALWAYS_DETAILED_SEC    = NO
INLINE_INHERITED_MEMB  = NO
FULL_PATH_NAMES        = NO
STRIP_FROM_PATH        = 
STRIP_FROM_INC_PATH    = 
SHORT_NAMES            = NO
JAVADOC_AUTOBRIEF      = NO
MULTILINE_CPP_IS_BRIEF = NO
INHERIT_DOCS           = YES
SEPARATE_MEMBER_PAGES  = NO
TAB_SIZE               = 8
OPTIMIZE_OUTPUT_FOR_C  = NO
OPTIMIZE_OUTPUT_JAVA   = NO
BUILTIN_STL_SUPPORT    = NO
DISTRIBUTE_GROUP_DOC   = NO
SUBGROUPING            = YES
#---------------------------------------------------------------------------
# Build related configuration options
#---------------------------------------------------------------------------
EXTRACT_ALL            = NO
EXTRACT_PRIVATE        = YES
EXTRACT_STATIC         = YES
EXTRACT_LOCAL_CLASSES  = YES
EXTRACT_LOCAL_METHODS  = NO
HIDE_UNDOC_MEMBERS     = NO
HIDE_UNDOC_CLASSES     = NO
HIDE_FRIEND_COMPOUNDS  = YES
HIDE_IN_BODY_DOCS      = NO
INTERNAL_DOCS          = YES
CASE_SENSE_NAMES       = YES
HIDE_SCOPE_NAMES       = NO
SHOW_INCLUDE_FILES     = YES
INLINE_INFO            = YES
SORT_MEMBER_DOCS       = YES
SORT_MEMBERS_CTORS_1ST = YES
SORT_BRIEF_DOCS        = YES
SORT_BY_SCOPE_NAME     = NO
GENERATE_TODOLIST      = YES
GENERATE_TESTLIST      = YES
GENERATE_BUGLIST       = YES
GENERATE_DEPRECATEDLIST = YES
ENABLED_SECTIONS       = 
MAX_INITIALIZER_LINES  = 30
SHOW_USED_FILES        = YES
SHOW_DIRECTORIES       = NO
FILE_VERSION_FILTER    = 
#---------------------------------------------------------------------------
# configuration options related to warning and progress messages
#---------------------------------------------------------------------------
QUIET                  = NO
WARNINGS               = YES
WARN_IF_UNDOCUMENTED   = YES
WARN_IF_DOC_ERROR      = YES
WARN_NO_PARAMDOC       = YES
WARN_FORMAT            = "\$file:\$line: \$text"
WARN_LOGFILE           =
#---------------------------------------------------------------------------
# configuration options related to the input files
#---------------------------------------------------------------------------
INPUT                  = .
FILE_PATTERNS          = *.cpp \
                         *.cc \
                         *.cxx \
                         *.h \
                         *.hh \
                         *.hxx \
                         *.hpp
RECURSIVE              = YES
EXCLUDE                = 
EXCLUDE_SYMLINKS       = NO
EXCLUDE_PATTERNS       = */.svn/* \
                         */.git/* \
                         */cmake/* \
                         *.moc.* \
                         moc* \
                         *.all_cpp.* \
                         *unload.* \
                         */test/* \
                         */tests/* \
                         *_p.cpp
EXAMPLE_PATH           = 
EXAMPLE_PATTERNS       = *
EXAMPLE_RECURSIVE      = NO
IMAGE_PATH             = $PWD
INPUT_FILTER           = 
FILTER_PATTERNS        = 
FILTER_SOURCE_FILES    = NO
#---------------------------------------------------------------------------
# configuration options related to the alphabetical class index
#---------------------------------------------------------------------------
ALPHABETICAL_INDEX     = NO
COLS_IN_ALPHA_INDEX    = 5
IGNORE_PREFIX          = 
#---------------------------------------------------------------------------
# do NOT generate any formats other than qhp
#---------------------------------------------------------------------------
SOURCE_BROWSER         = NO
GENERATE_HTML          = YES
GENERATE_LATEX         = NO
GENERATE_MAN           = NO
GENERATE_RTF           = NO
GENERATE_XML           = NO
GENERATE_AUTOGEN_DEF   = NO
GENERATE_PERLMOD       = NO
DISABLE_INDEX          = YES
#---------------------------------------------------------------------------
# configuration options related to the qhp output
#---------------------------------------------------------------------------
GENERATE_QHP           = YES
QHP_NAMESPACE          = org.kde.$virtual_folder
QHP_VIRTUAL_FOLDER     = $virtual_folder
QHG_LOCATION           = qhelpgenerator
#---------------------------------------------------------------------------
# Configuration options related to the preprocessor   
#---------------------------------------------------------------------------
ENABLE_PREPROCESSING   = YES
MACRO_EXPANSION        = YES
EXPAND_ONLY_PREDEF     = NO
SEARCH_INCLUDES        = YES
INCLUDE_PATH           = 
INCLUDE_FILE_PATTERNS  = 
PREDEFINED             = 
EXPAND_AS_DEFINED      = 
SKIP_FUNCTION_MACROS   = YES
#---------------------------------------------------------------------------
# Configuration::additions related to external references   
#---------------------------------------------------------------------------
ALLEXTERNALS           = NO
EXTERNAL_GROUPS        = YES
PERL_PATH              = /usr/bin/perl
#---------------------------------------------------------------------------
# Configuration options related to the dot tool   
#---------------------------------------------------------------------------
CLASS_DIAGRAMS         = NO
HIDE_UNDOC_RELATIONS   = YES
HAVE_DOT               = NO
CLASS_GRAPH            = NO
COLLABORATION_GRAPH    = NO
GROUP_GRAPHS           = NO
UML_LOOK               = NO
TEMPLATE_RELATIONS     = NO
INCLUDE_GRAPH          = NO
INCLUDED_BY_GRAPH      = YES
CALL_GRAPH             = NO
CALLER_GRAPH           = NO
GRAPHICAL_HIERARCHY    = NO
DIRECTORY_GRAPH        = NO
GENERATE_LEGEND        = NO
#---------------------------------------------------------------------------
# Configuration::additions related to the search engine   
#---------------------------------------------------------------------------
SEARCHENGINE           = NO


### KDE Settings
ALIASES = \
	"intern=\par<b>Internal use only.</b>" \
	"reimp=\par<b>Reimplemented from superclass.</b>" \
	"obsolete=@deprecated" \
	"feature=\xrefitem features \"Feature(s)\" \"Features\"" \
	"maintainer=\xrefitem maintainers \"Maintainer(s)\" \"Maintainers\"" \
	"unmaintained=\xrefitem unmaintained \"Unmaintained\" \"Unmaintained\"" \
	"requirement=\xrefitem requirements \"Requirement(s)\" \"Requirements\"" \
	"faq=\xrefitem FAQ \"F.A.Q.\" \"F.A.Q.\"" \
	"authors=\xrefitem authors \"Author(s)\" \"Authors\"" \
	"maintainers=\xrefitem maintainers \"Maintainer(s)\" \"Maintainers\"" \
	"port4=\xrefitem port4 \"KDE 4 Porting Guide\" \"KDE 4 Porting Guide\"" \
	"glossary=\xrefitem glossary \"KDE 4 Glossary\" \"KDE 4 Glossary\"" \
        "acronym=\b "\
	"licenses=\xrefitem licenses \"License(s)\" \"Licenses\"" \
	"short=@brief "\
	"FIXME=\xrefitem fixme \"Fixme\" \"Fixme\"" \
	"bc=\xrefitem bc \"Binary Compatible\" \"Binary Compatible\"" \
	"artistic=<a href=\"http://www.opensource.org/licenses/artistic-license.php\">Artistic</a>" \
	"bsd=<a href=\"http://www.xfree86.org/3.3.6/COPYRIGHT2.html#5\">BSD</a>" \
	"x11=<a href=\"http://www.xfree86.org/3.3.6/COPYRIGHT2.html#3\">X11</a>" \
	"gpl=<a href=\"http://www.fsf.org/licensing/licenses/gpl.html#SEC1\">GPL</a>" \
	"lgpl=<a href=\"http://www.fsf.org/licensing/licenses/lgpl.html#SEC1\">LGPL</a>" \
	"qpl=<a href=\"http://www.trolltech.com/products/qt/licenses\">QPL</a>"

PREDEFINED = DOXYGEN_SHOULD_SKIP_THIS \
        AKONADI_CONTACT_EXPORT="" \
        AKONADI_CONTACT_TESTS_EXPORT="" \
        AKONADI_EXPORT="" \
        AKONADI_FILESTORE_EXPORT="" \
        AKONADI_KABC_EXPORT="" \
        AKONADI_KCAL_EXPORT="" \
        AKONADI_KCAL_NEXT_EXPORT="" \
        AKONADI_KMIME_EXPORT="" \
        AKONADI_KMIME_TEST_EXPORT="" \
        AKONADI_NEXT_EXPORT="" \
        AKONADI_TESTS_EXPORT="" \
        AKONADI_XML_EXPORT="" \
        AKREGATOR_EXPORT="" \
        AKREGATORINTERFACES_EXPORT="" \
        AKREGATORPART_EXPORT="" \
        BTCORE_EXPORT="" \
        CANTOR_EXPORT="" \
        COMPOUNDVIEWER_EXPORT="" \
        EVENTVIEWS_EXPORT="" \
        GEOLOCATION_EXPORT="" \
        GPGMEPP_EXPORT="" \
        GWENVIEWLIB_EXPORT="" \
        GWSOAP_EXPORT="" \
        INCIDENCEEDITORS_EXPORT="" \
        INCIDENCEEDITORS_NG_EXPORT="" \
        ION_EXPORT="" \
        JABBER_EXPORT="" \
        KABC_DIR_EXPORT="" \
        KABC_EXPORT="" \
        KABC_FILE_CORE_EXPORT="" \
        KABC_GROUPWISE_EXPORT="" \
        KABC_LDAPKIO_EXPORT="" \
        KABC_NET_EXPORT="" \
        KADDRESSBOOK_EXPORT="" \
        KALARM_CAL_EXPORT="" \
        KALARM_RESOURCES_EXPORT="" \
        KASTENCONTROLLERS_EXPORT="" \
        KASTENCORE_EXPORT="" \
        KASTENGUI_EXPORT="" \
        KATEINTERFACES_EXPORT="" \
        KATEPART_TESTS_EXPORT="" \
        KAUDIODEVICELIST_EXPORT="" \
        KBLOG_EXPORT="" \
        KBOOKMARKMODEL_EXPORT="" \
        KCALCORE_EXPORT="" \
        KCALCORE_TEST_EXPORT="" \
        KCAL_EXPORT="" \
        KCAL_GROUPWISE_EXPORT="" \
        KCAL_RESOURCEBLOG_EXPORT="" \
        KCAL_RESOURCEREMOTE_EXPORT="" \
        KCAL_TEST_EXPORT="" \
        KCALUTILS_EXPORT="" \
        KCALUTILS_TEST_EXPORT="" \
        KCDDB_EXPORT="" \
        KCM_KNODE_EXPORT="" \
        KCM_KORGANIZER_EXPORT="" \
        KCM_KRESOURCES_EXPORT="" \
        KCMUTILS_EXPORT="" \
        KDE3SUPPORT_EXPORT="" \
        KDECORE_EXPORT="" \
        KDEEDUUI_EXPORT="" \
        KDE_EXPORT="" \
        KDEGAMES_EXPORT="" \
        KDEMULTIMEDIA_EXPORT="" \
        KDEPIM_COPY_EXPORT="" \
        KDEPIMDBUSINTERFACES_EXPORT="" \
        KDEPIM_EXPORT="" \
        KDESU_EXPORT="" \
        KDEUI_EXPORT="" \
        KDEWEBKIT_EXPORT="" \
        KDNSSD_EXPORT="" \
        KEDUVOCDOCUMENT_EXPORT="" \
        KEMOTICONS_EXPORT="" \
        KEPHAL_EXPORT="" \
        KERFUFFLE_EXPORT="" \
        KEXIV2_EXPORT="" \
        KFILE_EXPORT="" \
        KFONTINST_EXPORT="" \
        KGET_EXPORT="" \
        KGGZGAMES_EXPORT="" \
        KGGZMOD_EXPORT="" \
        KGGZNET_EXPORT="" \
        KHOLIDAYS_EXPORT="" \
        KHTML_EXPORT="" \
        KICKOFF_EXPORT="" \
        KIDLETIME_EXPORT="" \
        KIMAP_EXPORT="" \
        KIMPANELRUNTIME_EXPORT="" \
        KIMPROXY_EXPORT="" \
        KIO_EXPORT="" \
        KIOSLAVE_FILE_EXPORT="" \
        KIRCCLIENT_EXPORT="" \
        KIRC_EXPORT="" \
        KJSAPI_EXPORT="" \
        KLDAP_EXPORT="" \
        KLEO_EXPORT="" \
        KLEOPATRACLIENTCORE_EXPORT="" \
        KLEOPATRACLIENTGUI_EXPORT="" \
        KLINKSTATUS_EXPORT="" \
        KMAHJONGGLIB_EXPORT="" \
        KMAIL_EXPORT="" \
        KMEDIAPLAYER_EXPORT="" \
        KMIME_EXPORT="" \
        KMINDEXREADER_EXPORT="" \
        KNEWSTUFF_EXPORT="" \
        KNODE_EXPORT="" \
        KNOTIFYCONFIG_EXPORT="" \
        KNTLM_EXPORT="" \
        KOLFLIB_EXPORT="" \
        KOLOURPAINT_LGPL_EXPORT="" \
        KOMMANDERCORE_EXPORT="" \
        KOMMANDERWIDGETS_EXPORT="" \
        KONQ_TESTS_EXPORT="" \
        KONQUERORPRIVATE_EXPORT="" \
        KONSOLEPRIVATE_EXPORT="" \
        KONTACT_EXPORT="" \
        KONTACTINTERFACE_EXPORT="" \
        KOPETEADDACCOUNTWIZARD_EXPORT="" \
        KOPETECHATWINDOW_SHARED_EXPORT="" \
        KOPETE_CONTACT_LIST_EXPORT="" \
        KOPETE_EXPORT="" \
        KOPETE_IDENTITY_EXPORT="" \
        KOPETE_OTR_SHARED_EXPORT="" \
        KOPETEPRIVACY_EXPORT="" \
        KOPETE_STATUSMENU_EXPORT="" \
        KORGANIZER_CALENDAR_EXPORT="" \
        KORGANIZER_CORE_EXPORT="" \
        KORGANIZER_EVENTVIEWER_EXPORT="" \
        KORGANIZER_INTERFACES_EXPORT="" \
        KORGANIZERPRIVATE_EXPORT="" \
        KORG_STDPRINTING_EXPORT="" \
        KPARTS_EXPORT="" \
        KPGP_EXPORT="" \
        KPIMIDENTITIES_EXPORT="" \
        KPIMTEXTEDIT_EXPORT="" \
        KPIMUTILS_EXPORT="" \
        KPRINTUTILS_EXPORT="" \
        KPTY_EXPORT="" \
        KRESOURCES_EXPORT="" \
        KRICHTEXTEDITPART_EXPORT="" \
        KROSSCORE_EXPORT="" \
        KROSS_EXPORT="" \
        KROSSUI_EXPORT="" \
        KSIEVE_EXPORT="" \
        KSPEECH_EXPORT="" \
        KTEXTEDITOR_CODESNIPPETS_CORE_EXPORT="" \
        KTEXTEDITOR_EXPORT="" \
        KTNEF_EXPORT="" \
        KUNITCONVERSION_EXPORT="" \
        KUNITTEST_EXPORT="" \
        KUPNP_EXPORT="" \
        KWALLETBACKEND_EXPORT="" \
        KXMLRPCCLIENT_EXPORT="" \
        LANCELOT_EXPORT="" \
        LIBDOLPHINPRIVATE_EXPORT="" \
        LIBGROUPWISE_EXPORT="" \
        LIBKCARDGAME_EXPORT="" \
        LIBKDCRAW_EXPORT="" \
        LIBKIPI_EXPORT="" \
        LIBKONQ_EXPORT="" \
        LIBKSANE_EXPORT="" \
        LIBKYAHOO_EXPORT="" \
        LIBOSCAR_EXPORT="" \
        MAILDIR_EXPORT="" \
        MAILTRANSPORT_EXPORT="" \
        MBOX_EXPORT="" \
        MESSAGECOMPOSER_EXPORT="" \
        MESSAGECORE_EXPORT="" \
        MESSAGELIST_EXPORT="" \
        MESSAGEVIEWER_EXPORT="" \
        MICROBLOG_EXPORT="" \
        MOBILEUI_EXPORT="" \
        MOLLETNETWORK_EXPORT="" \
        NEPOMUK_EXPORT="" \
        NEPOMUKQUERY_EXPORT="" \
        OKTETACORE_EXPORT="" \
        OKTETAGUI_EXPORT="" \
        OKTETAKASTENCONTROLLERS_EXPORT="" \
        OKTETAKASTENCORE_EXPORT="" \
        OKTETAKASTENGUI_EXPORT="" \
        OKULAR_EXPORT="" \
        OSCAR_EXPORT="" \
        PIMSTRIGI_ANALYZER_EXPORT="" \
        PLASMACLOCK_EXPORT="" \
        PLASMA_COMIC_EXPORT="" \
        PLASMA_EXPORT="" \
        PLASMAGENERICSHELL_EXPORT="" \
        PLASMA_POTD_EXPORT="" \
        PLASMAWEATHER_EXPORT="" \
        PROXYMODELTESTSUITE_EXPORT="" \
        QGPGME_EXPORT="" \
        QQ_EXPORT="" \
        RTM_EXPORT="" \
        SCIENCE_EXPORT="" \
        SM_EXPORT="" \
        SOLIDCONTROL_EXPORT="" \
        SOLIDCONTROLIFACES_EXPORT="" \
        SUPERKARAMBA_EXPORT="" \
        SYNDICATION_EXPORT="" \
        SYSTEMSETTINGSVIEW_EXPORT="" \
        TASKMANAGER_EXPORT="" \
        TEMPLATEPARSER_EXPORT="" \
        THREADWEAVER_EXPORT="" \
        YAHOO_EXPORT="" \
\
        AKONADI_EXPORT_DEPRECATED="" \
        AKONADI_KCAL_NEXT_EXPORT_DEPRECATED="" \
        AKONADI_KMIME_EXPORT_DEPRECATED="" \
        AKONADI_NEXT_EXPORT_DEPRECATED="" \
        AKREGATOR_EXPORT_DEPRECATED="" \
        AKREGATORINTERFACES_EXPORT_DEPRECATED="" \
        BTCORE_EXPORT_DEPRECATED="" \
        CANTOR_EXPORT_DEPRECATED="" \
        COMPOUNDVIEWER_EXPORT_DEPRECATED="" \
        EVENTVIEWS_EXPORT_DEPRECATED="" \
        GEOLOCATION_EXPORT_DEPRECATED="" \
        GPGMEPP_EXPORT_DEPRECATED="" \
        INCIDENCEEDITORS_EXPORT_DEPRECATED="" \
        INCIDENCEEDITORS_NG_EXPORT_DEPRECATED="" \
        ION_EXPORT_DEPRECATED="" \
        KABC_DIRECTORY_EXPORT_DEPRECATED="" \
        KABC_EXPORT_DEPRECATED="" \
        KABC_FILE_CORE_EXPORT_DEPRECATED="" \
        KABC_GROUPWISE_EXPORT_DEPRECATED="" \
        KABC_LDAPKIO_EXPORT_DEPRECATED="" \
        KABC_NET_EXPORT_DEPRECATED="" \
        KADDRESSBOOK_EXPORT_DEPRECATED="" \
        KALARM_CAL_EXPORT_DEPRECATED="" \
        KALARM_RESOURCES_EXPORT_DEPRECATED="" \
        KASTENCONTROLLER_EXPORT_DEPRECATED="" \
        KASTENCORE_EXPORT_DEPRECATED="" \
        KASTENGUI_EXPORT_DEPRECATED="" \
        KAUDIODEVICELIST_EXPORT_DEPRECATED="" \
        KBOOKMARKMODEL_EXPORT_DEPRECATED="" \
        KCALCORE_EXPORT_DEPRECATED="" \
        KCAL_EXPORT_DEPRECATED="" \
        KCAL_GROUPWISE_EXPORT_DEPRECATED="" \
        KCAL_RESOURCEREMOTE_EXPORT_DEPRECATED="" \
        KCALUTILS_EXPORT_DEPRECATED="" \
        KCDDB_EXPORT_DEPRECATED="" \
        KCM_KORGANIZER_EXPORT_DEPRECATED="" \
        KCM_KRESOURCES_EXPORT_DEPRECATED="" \
        KDE3SUPPORT_EXPORT_DEPRECATED="" \
        KDECORE_EXPORT_DEPRECATED="" \
        KDE_DEPRECATED="" \
        KDEEDUUI_EXPORT_DEPRECATED="" \
        KDEGAMES_EXPORT_DEPRECATED="" \
        KDEMULTIMEDIA_EXPORT_DEPRECATED="" \
        KDEPIM_COPY_EXPORT_DEPRECATED="" \
        KDEPIM_EXPORT_DEPRECATED="" \
        KDEUI_EXPORT_DEPRECATED="" \
        KDEWEBKIT_EXPORT_DEPRECATED="" \
        KEDUVOCDOCUMENT_EXPORT_DEPRECATED="" \
        KEPHAL_EXPORT_DEPRECATED="" \
        KERFUFFLE_EXPORT_DEPRECATED="" \
        KEXIV2_EXPORT_DEPRECATED="" \
        KFILE_EXPORT_DEPRECATED="" \
        KGGZGAMES_EXPORT_DEPRECATED="" \
        KGGZMOD_EXPORT_DEPRECATED="" \
        KGGZNET_EXPORT_DEPRECATED="" \
        KHOLIDAYS_EXPORT_DEPRECATED="" \
        KHTML_EXPORT_DEPRECATED="" \
        KICKOFF_EXPORT_DEPRECATED="" \
        KIMPANELRUNTIME_EXPORT_DEPRECATED="" \
        KIO_EXPORT_DEPRECATED="" \
        KIOSLAVE_FILE_EXPORT_DEPRECATED="" \
        KJSAPI_EXPORT_DEPRECATED="" \
        KLEO_EXPORT_DEPRECATED="" \
        KLINKSTATUS_EXPORT_DEPRECATED="" \
        KMAHJONGGLIB_EXPORT_DEPRECATED="" \
        KMAIL_EXPORT_DEPRECATED="" \
        KMIME_EXPORT_DEPRECATED="" \
        KNEWSTUFF_EXPORT_DEPRECATED="" \
        KNODE_EXPORT_DEPRECATED="" \
        KNTLM_EXPORT_DEPRECATED="" \
        KOLFLIB_EXPORT_DEPRECATED="" \
        KOLOURPAINT_LGPL_EXPORT_DEPRECATED="" \
        KOMMANDERCORE_EXPORT_DEPRECATED="" \
        KOMMANDERWIDGETS_EXPORT_DEPRECATED="" \
        KONQUERORPRIVATE_EXPORT_DEPRECATED="" \
        KONTACT_EXPORT_DEPRECATED="" \
        KORGANIZER_CALENDAR_EXPORT_DEPRECATED="" \
        KORGANIZER_CORE_EXPORT_DEPRECATED="" \
        KORGANIZER_EVENTVIEWER_EXPORT_DEPRECATED="" \
        KORGANIZER_INTERFACES_EXPORT_DEPRECATED="" \
        KORGANIZERPRIVATE_EXPORT_DEPRECATED="" \
        KORG_STDPRINTING_EXPORT_DEPRECATED="" \
        KPGP_EXPORT_DEPRECATED="" \
        KPINTERFACES_EXPORT_DEPRECATED="" \
        KPTY_EXPORT_DEPRECATED="" \
        KRESOURCES_EXPORT_DEPRECATED="" \
        KROSSCORE_EXPORT_DEPRECATED="" \
        KROSS_EXPORT_DEPRECATED="" \
        KSIEVE_EXPORT_DEPRECATED="" \
        KTEXTEDITOR_EXPORT_DEPRECATED="" \
        KTNEF_EXPORT_DEPRECATED="" \
        KUNITCONVERSION_EXPORT_DEPRECATED="" \
        KUNITTEST_EXPORT_DEPRECATED="" \
        KUPNP_EXPORT_DEPRECATED="" \
        LANCELOT_EXPORT_DEPRECATED="" \
        LIBKCARDGAME_EXPORT_DEPRECATED="" \
        LIBKDCRAW_EXPORT_DEPRECATED="" \
        LIBKIPI_EXPORT_DEPRECATED="" \
        LIBKONQ_EXPORT_DEPRECATED="" \
        LIBKSANE_EXPORT_DEPRECATED="" \
        MAILDIR_EXPORT_DEPRECATED="" \
        MAILTRANSPORT_EXPORT_DEPRECATED="" \
        MBOX_EXPORT_DEPRECATED="" \
        MOLLETNETWORK_EXPORT_DEPRECATED="" \
        NEPOMUKQUERY_EXPORT_DEPRECATED="" \
        NEPOMUK_SERVER_EXPORT_DEPRECATED="" \
        OKTETACORE_EXPORT_DEPRECATED="" \
        OKTETAGUI_EXPORT_DEPRECATED="" \
        OKTETAKASTENCONTROLLER_EXPORT_DEPRECATED="" \
        OKTETAKASTENCORE_EXPORT_DEPRECATED="" \
        OKTETAKASTENGUI_EXPORT_DEPRECATED="" \
        PLASMACLOCK_EXPORT_DEPRECATED="" \
        PLASMA_EXPORT_DEPRECATED="" \
        PLASMAGENERICSHELL_EXPORT_DEPRECATED="" \
        PLASMAWEATHER_EXPORT_DEPRECATED="" \
        QGPGME_EXPORT_DEPRECATED="" \
        RTM_EXPORT_DEPRECATED="" \
        SCIENCE_EXPORT_DEPRECATED="" \
        SM_EXPORT_DEPRECATED="" \
        SOLID_EXPORT_DEPRECATED="" \
        SUPERKARAMBA_EXPORT_DEPRECATED="" \
        SYSTEMSETTINGSVIEW_EXPORT_DEPRECATED="" \
        TASKMANAGER_EXPORT_DEPRECATED="" \
\
        Q_WS_X11="" \
        Q_WS_WIN="" \
        Q_WS_MAC="" \
        Q_WS_QWS="" \
        Q_WS_MAEMO_5="" \
        Q_OS_LINUX="" \
	Q_OS_UNIX="" \
        Q_OS_WIN="" \
        Q_OS_MAC="" \
        Q_OS_MACX="" \
        Q_OS_DARWIN="" \
        Q_OS_FREEBSD="" \
        Q_OS_NETBSD="" \
        Q_OS_OPENBSD="" \
        Q_OS_BSD4="" \
        Q_OS_SOLARIS="" \
        Q_OS_IRIX="" \
\
	Q_SLOTS="slots" \
	Q_SIGNALS="signals"

EOF

