#!/usr/bin/perl -w

# Alex Merry <alex.merry@kde.org> 2015
#
# Modifies CMakeLists.txt in current directory to use the new (as of
# extra-cmake-modules 1.6.0) install variables.
#
# Do not use unless your project depends on extra-cmake-modules 1.6.0
# or later.

use strict;
my $file = "CMakeLists.txt";

my %old_var_map = (
    BUNDLE_INSTALL_DIR => 'BUNDLEDIR',
    EXEC_INSTALL_PREFIX => 'EXECROOTDIR',
    BIN_INSTALL_DIR => 'BINDIR',
    SBIN_INSTALL_DIR => 'SBINDIR',
    LIB_INSTALL_DIR => 'LIBDIR',
    LIBEXEC_INSTALL_DIR => 'LIBEXECDIR',
    KF5_LIBEXEC_INSTALL_DIR => 'LIBEXECDIR_KF5',
    CMAKECONFIG_INSTALL_PREFIX => 'CMAKEPACKAGEDIR',
    QT_PLUGIN_INSTALL_DIR => 'QTPLUGINDIR',
    PLUGIN_INSTALL_DIR => 'PLUGINDIR',
    IMPORTS_INSTALL_DIR => 'QTQUICKIMPORTSDIR',
    QML_INSTALL_DIR => 'QMLDIR',
    INCLUDE_INSTALL_DIR => 'INCLUDEDIR',
    KF5_INCLUDE_INSTALL_DIR => 'INCLUDEDIR_KF5',
    DATA_INSTALL_DIR => 'DATADIR',
    KF5_DATA_INSTALL_DIR => 'DATADIR_KF5',
    HTML_INSTALL_DIR => 'DOCBUNDLEDIR',
    KCFG_INSTALL_DIR => 'KCFGDIR',
    KCONF_UPDATE_INSTALL_DIR => 'KCONFUPDATEDIR',
    SERVICES_INSTALL_DIR => 'KSERVICES5DIR',
    SERVICETYPES_INSTALL_DIR => 'KSERVICETYPES5DIR',
    KXMLGUI_INSTALL_DIR => 'KXMLGUI5DIR',
    KNOTIFYRC_INSTALL_DIR => 'KNOTIFY5RCDIR',
    ICON_INSTALL_DIR => 'ICONDIR',
    LOCALE_INSTALL_DIR => 'LOCALEDIR',
    SOUND_INSTALL_DIR => 'SOUNDDIR',
    TEMPLATES_INSTALL_DIR => 'TEMPLATEDIR',
    WALLPAPER_INSTALL_DIR => 'WALLPAPERDIR',
    XDG_APPS_INSTALL_DIR => 'APPDIR',
    XDG_DIRECTORY_INSTALL_DIR => 'DESKTOPDIR',
    XDG_MIME_INSTALL_DIR => 'MIMEDIR',
    MAN_INSTALL_DIR => 'MANDIR',
    DBUS_INTERFACES_INSTALL_DIR => 'DBUSINTERFACEDIR',
    DBUS_SERVICES_INSTALL_DIR => 'DBUSSERVICEDIR',
    DBUS_SYSTEM_SERVICES_INSTALL_DIR => 'DBUSSYSTEMSERVICEDIR',
    SYSCONF_INSTALL_DIR => 'SYSCONFDIR',
    CONFIG_INSTALL_DIR => 'CONFDIR',
    AUTOSTART_INSTALL_DIR => 'AUTOSTARTDIR',
    SHARE_INSTALL_PREFIX => 'DATAROOTDIR',
);

my @new_var_suffixes = (
    'METAINFODIR',
    'INFODIR',
    'DBUSDIR',
    'LOCALSTATEDIR',
    'SHAREDSTATEDIR',
    'LIBEXECDIR_KF5',
);
push(@new_var_suffixes, values %old_var_map);

my %extra_changes = (
    INSTALL_TARGETS_DEFAULT_ARGS => 'KDE_INSTALL_TARGETS_DEFAULT_ARGS'
);

open(my $FILE, "<", $file) || die;
my $modified = 0;
my @l = map {
    my $orig = $_;
    my $updated = $_;

    # all variables we care about have "INSTALL" in their name
    if (/INSTALL/) {
        while (my ($oldvar,$suffix) = each(%old_var_map)) {
            $updated =~ s/\b$oldvar\b/KDE_INSTALL_$suffix/;
        }
        foreach (@new_var_suffixes) {
            $updated =~ s/\bCMAKE_INSTALL_$_\b/KDE_INSTALL_$_/;
        }
        while (my ($oldvalue,$newvalue) = each(%extra_changes)) {
            $updated =~ s/\b$oldvalue\b/$newvalue/;
        }
    }

    $modified ||= $orig ne $updated;
    $updated;
} <$FILE>;

if ($modified) {
    open (my $OUT, ">", $file);
    print $OUT @l;
    close ($OUT);
}
# vi:et:sts=4:sw=4
