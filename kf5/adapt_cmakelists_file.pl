#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> 2014
# Modifies CMakeLists.txt in current directory to use kf5 macro

use strict;
my $file = "CMakeLists.txt";
open(my $FILE, "<", $file) || die;
my $modified = 0;
my @l = map {
  my $orig = $_;
  if (/kde4_install_icons/) {
     $_ =~ s/kde4_install_icons/ecm_install_icons/;
     $modified = 1;
  }
  if (/kde4_add_library/) {
     $_ =~ s/kde4_add_library/add_library/;
     $modified = 1;
  }
  if (/kde4_add_ui_files/) {
     $_ =~ s/kde4_add_ui_files/qt5_wrap_ui/;
     $modified = 1;
  }
  if (/kde4_add_kcfg_files/) {
      $_ =~ s/kde4_add_kcfg_files/kconfig_add_kcfg_files/;
      $modified = 1;
  }
  if (/kde4_add_executable/i) {
      $_ =~ s/kde4_add_executable/add_executable/i;
      $modified = 1;
  }

  if (/KDE4_ENABLE_EXCEPTIONS/) {
      $_ =~ s/set\s*\(\s*CMAKE_CXX_FLAGS\s*\"\$\{CMAKE_CXX_FLAGS\} \$\{KDE4_ENABLE_EXCEPTIONS\}\"\s*\)/kde_enable_exceptions\(\)/;
      $modified = 1;
  } 
  if (/qt4_add_dbus_adaptor/) {
      $_ =~ s/qt4_add_dbus_adaptor/qt5_add_dbus_adaptor/;
      $modified = 1;
  }
  if (/qt4_wrap_ui/) {
      $_ =~ s/qt4_wrap_ui/qt5_wrap_ui/;
      $modified = 1;
  }
  if (/KDEPIMLIBS_AKONADI_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_AKONADI_LIBS}/KF5::AkonadiCore/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KCALCORE_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KCALCORE_LIBS}/KF5::CalendarCore/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KPIMUTILS_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KPIMUTILS_LIBS}/KF5::PimUtils/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_MAILTRANSPORT_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_MAILTRANSPORT_LIBS}/KF5::MailTransport/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KMIME_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KMIME_LIBS}/KF5::Mime/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KPIMIDENTITIES_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KPIMIDENTITIES_LIBS}/KF5::PimIdentities/;
     $modified = 1;
  }
  if (/KDE4_KIO_LIBS/) {
     $_ =~ s/\${KDE4_KIO_LIBS}/KF5::KIOCore/;
     $modified = 1;
  }
  if (/KDE4_KROSSCORE_LIBS/) {
     $_ =~ s/\${KDE4_KROSSCORE_LIBS}/KF5::KrossCore/;
     $modified = 1;
  }
  if (/QT_QTDBUS_LIBRARY/) {
     $_ =~ s/\${QT_QTDBUS_LIBRARY}//;
     $modified = 1;
  }
  if (/QT_QTCORE_LIBRARY/) {
     $_ =~ s/\${QT_QTCORE_LIBRARY}//;
     $modified = 1;
  }
  if (/QT_QTGUI_LIBRARY/) {
     $_ =~ s/\${QT_QTGUI_LIBRARY}//;
     $modified = 1;
  }
  if (/QT_QTNETWORK_LIBRARY/) {
     $_ =~ s/\${QT_QTNETWORK_LIBRARY}//;
     $modified = 1;
  }
  if (/KDE4_KDECORE_LIBS/) {
     $_ =~ s/\${KDE4_KDECORE_LIBS}//;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KIMAP_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KIMAP_LIBS}/KF5::Imap/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_AKONADI_KMIME_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_AKONADI_KMIME_LIBS}/KF5::AkonadiMime/;
     $modified = 1;
  }
  if (/KDE4_KNOTIFYCONFIG_LIBS/) {
     $_ =~ s/\${KDE4_KNOTIFYCONFIG_LIBS}/KF5::NotifyConfig/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KPIMTEXTEDIT_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KPIMTEXTEDIT_LIBS}/KF5::PimTextEdit/;
     $modified = 1;
  }
  if (/KDE4_KDEWEBKIT_LIBRARY/) {
     $_ =~ s/\${KDE4_KDEWEBKIT_LIBRARY}/KF5::WebKit/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KMBOX_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KMBOX_LIBS}/KF5::Mbox/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KALARMCAL_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KALARMCAL_LIBS}/KF5::AlarmCalendar/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KABC_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KABC_LIBS}/KF5::Abc/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_AKONADI_CONTACT_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_AKONADI_CONTACT_LIBS}/KF5::AkonadiContact/;
     $modified = 1;
  }
  if (/KDE4_KDEUI_LIBS/) {
     $_ =~ s/\${KDE4_KDEUI_LIBS}//;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KTNEF_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KTNEF_LIBS}/KF5::KTnef/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KBLOG_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KBLOG_LIBS}/KF5::Blog/;
     $modified = 1;
  }
  if (/KDE4_KNEWSTUFF3_LIBS/) {
     $_ =~ s/\${KDE4_KNEWSTUFF3_LIBS}/KF5::NewStuff/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KLDAP_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KLDAP_LIBS}/KF5::KLdap/;
     $modified = 1;
  }
  if (/BALOO_LIBRARIES/) {
     $_ =~ s/\${BALOO_LIBRARIES}/Baloo/;
     $modified = 1;
  }
  if (/KDE4_KCMUTILS_LIBS/) {
     $_ =~ s/\${KDE4_KCMUTILS_LIBS}/KF5::KCMUtils/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KCALUTILS_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KCALUTILS_LIBS}/KF5::CalendarUtils/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KHOLIDAYS_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KHOLIDAYS_LIBS}/KF5::Holidays/;
     $modified = 1;
  }
  if (/KDE4_KUTILS_LIBS/) {
     $_ =~ s/\${KDE4_KUTILS_LIBS}//;
     $modified = 1;
  }
  if (/KDE4_KDECORE_LIBRARY/) {
     $_ =~ s/\${KDE4_KDECORE_LIBRARY}//;
     $modified = 1;
  }
  if (/KDE4_KDEUI_LIBRARY/) {
     $_ =~ s/\${KDE4_KDEUI_LIBRARY}//;
     $modified = 1;
  }
  if (/KDE4_KTEXTEDITOR_LIBS/) {
     $_ =~ s/\${KDE4_KTEXTEDITOR_LIBS}/KF5::TextEditor/;
     $modified = 1;
  }
  if (/qt4_wrap_cpp/) {
     $_ =~ s/qt4_wrap_cpp/qt5_wrap_cpp/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_SYNDICATION_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_SYNDICATION_LIBS}/KF5::Syndication/;
     $modified = 1;
  }
  if (/KDE4_KHTML_LIBS/) {
     $_ =~ s/\${KDE4_KHTML_LIBS}/KF5::KHtml/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_KONTACTINTERFACE_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_KONTACTINTERFACE_LIBS}/KF5::KontactInterface/;
     $modified = 1;
  }
  if (/KDE4_KNOTIFYCONFIG_LIBRARY/) {
     $_ =~ s/\${KDE4_KNOTIFYCONFIG_LIBRARY}/KF5::NotifyConfig/;
     $modified = 1;
  }
  if (/QT_QTDECLARATIVE_LIBRARY/) {
     $_ =~ s/\${QT_QTDECLARATIVE_LIBRARY}/Qt5::Declarative/;
     $modified = 1;
  }
  if (/KDE4_KPARTS_LIBS/) {
     $_ =~ s/\${KDE4_KPARTS_LIBS}//;
     $modified = 1;
  }
  if (/KDE4_PHONON_LIBS/) {
     $_ =~ s/\${KDE4_PHONON_LIBS}/Phonon::phonon4qt5/;
     $modified = 1;
  }
  if (/QT_QTTEST_LIBRARY/) {
     $_ =~ s/\${QT_QTTEST_LIBRARY}/Qt5::Test/;
     $modified = 1;
  }

  if (/kde4_create_handbook/) {
     $_ =~ s/kde4_create_handbook/kdoctools_create_handbook/;
     $modified = 1;
  }
  if (/kde4_create_manpage/) {
     $_ =~ s/kde4_create_manpage/kdoctools_create_manpage/;
     $modified = 1;
  }
  if (/KDEPIMLIBS_MICROBLOG_LIBS/) {
     $_ =~ s/\${KDEPIMLIBS_MICROBLOG_LIBS}/KF5::MicroBlog/;
     $modified = 1;
  }
  if (/KDE4_SOLID_LIBS/) {
     $_ =~ s/\${KDE4_SOLID_LIBS}//;
     $modified = 1;     
  }
  if (/QT_QTWEBKIT_LIBRARY/) {
     $_ =~ s/\${QT_QTWEBKIT_LIBRARY}/Qt5::WebKitWidgets/;
     $modified = 1;
  }
  if (/macro_optional_add_subdirectory/) {
     $_ =~ s/macro_optional_add_subdirectory/add_subdirectory/;
     $modified = 1;
  }

  if (/qt4_add_dbus_interfaces/) {
     $_ =~ s/qt4_add_dbus_interfaces/qt5_add_dbus_interfaces/;
     $modified = 1;
  }
  if (/qt4_add_dbus_interface/) {
     $_ =~ s/qt4_add_dbus_interface/qt5_add_dbus_interface/;
     $modified = 1;
  }
  if (/qt4_generate_moc/) {
     $_ =~ s/qt4_generate_moc/qt5_generate_moc/;
     $modified = 1;
  }
  if (/qt4_generate_dbus_interface/) {
     $_ =~ s/qt4_generate_dbus_interface/qt5_generate_dbus_interface/;
     $modified = 1;
  }
  if (/\.notifyrc/) {
     my $regexp = qr/
                  ^(\s*install\s*\(\s*FILES\s+[^\s)]+\.notifyrc\s+DESTINATION\s+)
                  \$\{DATA_INSTALL_DIR\}\/[^\s)]+
                  (.*)$
                  /x; # /x Enables extended whitespace mode
     if (my ($begin, $end) = $_ =~ $regexp) {
        $_ = $begin . "\${KNOTIFYRC_INSTALL_DIR}" . $end . "\n";
        $modified = 1;
     } elsif (not /KNOTIFYRC_INSTALL_DIR/) {
        my $line = $_;
        $line =~ s/\s*$//;
        print "Could not fix a .notifyrc file installation call ($line)\n"
     }
  }

  #kde4_add_plugin(kio_mbox ${kio_mbox_PART_SRCS})
  my $regexp = qr/
               ^(\s*)                  # (1) Indentation
               kde4_add_plugin\s*\(    # 
               \s*([^ ]*)\s*           # (2) libname
               (.*)$                   # (3) end
               /x; # /x Enables extended whitespace mode
  if (my ($indent, $libname, $end) = $_ =~ $regexp) {
     $_ = $indent . "add_library($libname MODULE " . $end . "\n";
     $modified = 1;
  }
  $modified ||= $orig ne $_;
  $_;
} <$FILE>;

if ($modified) {
    open (my $OUT, ">", $file);
    print $OUT @l;
    close ($OUT);
}
