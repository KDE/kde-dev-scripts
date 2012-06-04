#!/usr/bin/perl -w

# David Faure <faure@kde.org>
# KStandardDirs -> QStandardPaths

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

my %easyResource = (
   "data" =>   "QStandardPaths::GenericDataLocation",
   "appdata" =>   "QStandardPaths::DataLocation",
   "config" =>   "QStandardPaths::ConfigLocation",
   "xdgdata-apps" =>   "QStandardPaths::ApplicationsLocation",
   "cache" =>   "QStandardPaths::CacheLocation"
);

my %otherResource = (
   "services" => "kde5/services",
   "xdgdata-icon" => "icons",
   "icon" => "icons",
   "xdgdata-mime" => "mime"
);

my %xdgconfResource = (
   "xdgconf-menu" => "menus",
   "xdgconf-autostart" => "autostart",
   "autostart" => "autostart"
);

foreach my $file (@ARGV) {
    my $addconfigprefix = 0;

    functionUtilkde::substInFile {
        s/KGlobal::dirs\(\)->findResource/KStandardDirs::locate/;
        s/KGlobal::dirs\(\)->locate/KStandardDirs::locate/;
        s/KGlobal::dirs\(\)->findExe/KStandardDirs::findExe/;
        s/KStandardDirs::locate\("exe", /KStandardDirs::findExe\(/;
        if (/KStandardDirs::locateLocal\(\s*\"(.*)\",\s*(.*)\s*\)/) {
            my $resource = $1;
            my $fileName = $2;
            my $loc;
            if (defined $easyResource{$resource}) {
                $loc = $easyResource{$resource};
                $fileName = "QLatin1Char('\/') + $fileName";
            } elsif (defined $otherResource{$resource}) {
                $loc = "QStandardPaths::GenericDataLocation";
                $fileName = "QLatin1String(\"$otherResource{$resource}\/\") + $fileName";
            } elsif (defined $xdgconfResource{$resource}) {
                $loc = "QStandardPaths::ConfigLocation";
                $fileName = "QLatin1String(\"$xdgconfResource{$resource}\/\") + $fileName";
            } else {
                print STDERR "Unhandled resource $resource\n";
            }
            if (defined $loc) {
                s/KStandardDirs::locateLocal\(.*\)/QStandardPaths::writableLocation($loc) + $fileName/;
            }
        }
        if (/KStandardDirs::locate\(\s*\"(.*)\",\s*(.*)\s*\)/) {
            my $resource = $1;
            my $fileName = $2;
            my $loc;
            if (defined $easyResource{$resource}) {
                $loc = $easyResource{$resource};
            } elsif (defined $otherResource{$resource}) {
                $loc = "QStandardPaths::GenericDataLocation";
                $fileName = "QLatin1String(\"$otherResource{$resource}\/\") + $fileName";
            } elsif (defined $xdgconfResource{$resource}) {
                $loc = "QStandardPaths::ConfigLocation";
                $fileName = "QLatin1String(\"$xdgconfResource{$resource}\/\") + $fileName";
            } else {
                print STDERR "Unhandled resource $resource\n";
            }
            if (defined $loc) {
                # ends with a '/' (in a string literal) ?
                print STDERR "fileName=$fileName\n";
                if ($fileName =~ s/\/\"$/\"/ || $fileName =~ s/\/\"\)$/\"\)/) {
                    s/KStandardDirs::locate\(.*\)/QStandardPaths::locate($loc, $fileName, QStandardPaths::LocateDirectory)/;
                } else {
                    s/KStandardDirs::locate\(.*\)/QStandardPaths::locate($loc, $fileName)/;
                }
            }
        }
        if (/KStandardDirs::findExe\(\s*\"(.*)\"(,\s*[^\)]*)?\s*\)/ ||
            /KStandardDirs::findExe\(\s*QLatin1String\s*\(\"(.*)\"\)(,\s*[^\)]*)?\s*\)/) {
            my $exe = $1;
            if (`which $exe 2>/dev/null`) {
                print STDERR "found $exe\n";
                s/KStandardDirs::findExe/QStandardPaths::findExecutable/;
            } else {
                if (`locate libexec/$exe`) {
                    s/KStandardDirs::findExe\([^\)]*\)/CMAKE_INSTALL_PREFIX \"\/\" LIBEXEC_INSTALL_DIR \"\/$exe\"/;
                    #print STDERR "$exe NOT found in PATH, use CMAKE_INSTALL_PREFIX \"/\" LIBEXEC_INSTALL_DIR \"/$exe\" instead\n";
                    $addconfigprefix = 1;
                } else {
                    print STDERR "$exe NOT found in PATH, nor in libexec, using locate. Where is it?\n";
                }
            }
        }
        if (/KGlobal::dirs\(\)->saveLocation\(\s*\"([^\"]*)\"(,\s*[^\)]*)?\s*\)/) {
            my $resource = $1;
            my $suffix = $2;
            #print STDERR "resource=$resource suffix=$suffix\n";
            my $loc;
            my $add = "";
            if (defined $easyResource{$resource}) {
                $loc = $easyResource{$resource};
            } elsif (defined $otherResource{$resource}) {
                $loc = "QStandardPaths::GenericDataLocation";
                $add = " + QLatin1String(\"$otherResource{$resource}\/\")";
            } elsif (defined $xdgconfResource{$resource}) {
                $loc = "QStandardPaths::ConfigLocation";
                $add = " + QLatin1String(\"$otherResource{$resource}\/\")";
            } else {
                print STDERR "Unhandled resource $resource for saveLocation:\n$_\n";
            }
            if ($suffix) {
                $suffix =~ s/,\s*//;
                $add .= " + '/' + $suffix";
                #print STDERR "loc=$loc add=$add\n";
            }
            s/KGlobal::dirs\(\)->saveLocation\(.*\)/QStandardPaths::writableLocation($loc)$add/ if ($loc);
        }
    } $file;
    functionUtilkde::addIncludeInFile($file, "config-prefix.h") if ($addconfigprefix);
    if (not `grep -i dirs $file | grep -v kstandarddirs\.h`) {
      functionUtilkde::removeIncludeInFile($file, "kstandarddirs.h");
    }
    if (`grep QStandardPaths $file | grep -v '#include'`) {
      functionUtilkde::addIncludeInFile($file, "qstandardpaths.h");
    }
}

functionUtilkde::diffFile( "@ARGV" );
