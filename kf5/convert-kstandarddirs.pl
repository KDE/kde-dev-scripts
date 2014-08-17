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
   "cache" =>   "QStandardPaths::CacheLocation",
   "socket" =>   "QStandardPaths::RuntimeLocation"
);

my %otherResource = (
   "services" => "kde5/services",
   "xdgdata-icon" => "icons",
   "icon" => "icons",
   "locale" => "locale",
   "wallpaper" => "wallpapers",
   "xdgdata-mime" => "mime",
   "sound" => "sound"
);

my %xdgconfResource = (
   "xdgconf-menu" => "menus",
   "xdgconf-autostart" => "autostart",
   "autostart" => "autostart"
);

sub locationAndSubdir
{
    my ($resource, $fileName) = @_;
    my $loc;
    if (defined $easyResource{$resource}) {
        $loc = $easyResource{$resource};
    } elsif (defined $otherResource{$resource}) {
        $loc = "QStandardPaths::GenericDataLocation";
        $fileName = $fileName eq "QString()" ? "" : " + $fileName";
        $fileName = "QLatin1String(\"$otherResource{$resource}\/\")$fileName";
    } elsif (defined $xdgconfResource{$resource}) {
        $loc = "QStandardPaths::ConfigLocation";
        $fileName = $fileName eq "QString()" ? "" : " + $fileName";
        $fileName = "QLatin1String(\"$xdgconfResource{$resource}\/\")$fileName";
    } else {
        print STDERR "Unhandled resource $resource\n";
    }
    return ($loc, $fileName);
}

foreach my $file (@ARGV) {
    my $addconfigprefix = 0;

    functionUtilkde::substInFile {
        s/KGlobal::dirs\(\)->findResource\b/KStandardDirs::locate/;
        s/KGlobal::dirs\(\)->locate/KStandardDirs::locate/;
        s/KGlobal::dirs\(\)->findExe/KStandardDirs::findExe/;
        s/KGlobal::dirs\(\)->localxdgdatadir\s*\(\)/QStandardPaths::writableLocation(QStandardPaths::GenericDataLocation) + QLatin1Char('\/')/;
        s/KGlobal::dirs\(\)->localxdgconfdir\s*\(\)/QStandardPaths::writableLocation(QStandardPaths::GenericConfigLocation) + QLatin1Char('\/')/;
        s/KStandardDirs::locate\s*\(\s*"exe", /KStandardDirs::findExe\(/;

        if (/KStandardDirs::locateLocal\s*\(\s*\"(.*)\",\s*(.*)\s*\)/) {
            my ($loc, $fileName) = locationAndSubdir($1, $2);
            if (defined $loc) {
                # prepend a slash
                if ($fileName =~ m/QLatin1String/) {
                    $fileName =~ s/QLatin1String\s*\(\s*\"/QLatin1String(\"\//;
                } else {
                    $fileName = "QLatin1Char('\/') + " . $fileName;
                }
                s/KStandardDirs::locateLocal\s*\(.*\)/QStandardPaths::writableLocation($loc) + $fileName/;
            }
        }
        if (/KStandardDirs::locate\s*\(\s*\"(.*)\",\s*(.*)\s*\)/ ||
            /KGlobal::dirs\(\)->findAllResources\s*\(\s*\"(.*)\",\s*(.*)\s*\)/ ||
            /KGlobal::dirs\(\)->findDirs\s*\(\s*\"(.*)\",\s*(.*)\s*\)/  ) {
            my ($loc, $fileName) = locationAndSubdir($1, $2);
            if (defined $loc) {
                # ends with a '/' (in a string literal) ?
                #print STDERR "fileName=$fileName\n";
                my ($search, $replace);
                if (/KStandardDirs::locate/) {
                   $search = qr/KStandardDirs::locate\s*/;
                   $replace = "QStandardPaths::locate";
                } elsif (/KGlobal::dirs\(\)->findAllResources/) {
                   $search = qr/KGlobal::dirs\(\)->findAllResources\s*/;
                   $replace = "QStandardPaths::locateAll";
                } elsif (/KGlobal::dirs\(\)->findDirs/) {
                   $search = qr/KGlobal::dirs\(\)->findDirs\s*/;
                   $replace = "QStandardPaths::locateAll";
                }
                if ($fileName =~ s/\/\"$/\"/ || $fileName =~ s/\/\"\)$/\"\)/) {
                    s/$search\(.*\)/$replace($loc, $fileName, QStandardPaths::LocateDirectory)/;
                } else {
                    s/$search\(.*\)/$replace($loc, $fileName)/;
                }
            }
        }
        my $regexp = qr/
           ^(\s*)                                  # (1) Indentation
           (.*?)                                   # (2) Possibly "Classname " (the ? means non-greedy)
           (\w+)                                   # (3) variable name
           \s*=\s*                                 #   assignment
           KGlobal::dirs\(\)->findResourceDir\s*\(\s* #  KGlobal::dirs\(\)->findResourceDir\(
           \"(.*)\"\s*,                            # (4) location
           \s*(.*)\s*\)                            # (5) subdir
           (.*)$                                   # (6) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $var, $location, $subdir, $after) = $_ =~ $regexp) {
           my ($loc, $fileName) = locationAndSubdir($location, $subdir);
           if (defined $loc) {
               warn "$loc, $fileName\n";
               $_ = $indent . $left . $var . "= QStandardPaths::locate($loc, $fileName)" . $after . "\n";
               $_ .= $indent . "$var = QFileInfo($var).absolutePath();\n";
           }
        }
       

 
        if (/KStandardDirs::findExe\s*\(\s*\"(.*)\"(,\s*[^\)]*)?\s*\)/ ||
            /KStandardDirs::findExe\s*\(\s*QLatin1String\s*\(\"(.*)\"\)(,\s*[^\)]*)?\s*\)/) {
            my $exe = $1;
            if (`which $exe 2>/dev/null`) {
                print STDERR "found $exe\n";
                s/KStandardDirs::findExe/QStandardPaths::findExecutable/;
            } else {
                if (`locate libexec/$exe`) {
                    s/KStandardDirs::findExe\s*\([^\)]*\)/CMAKE_INSTALL_PREFIX \"\/\" LIBEXEC_INSTALL_DIR \"\/$exe\"/;
                    #print STDERR "$exe NOT found in PATH, use CMAKE_INSTALL_PREFIX \"/\" LIBEXEC_INSTALL_DIR \"/$exe\" instead\n";
                    $addconfigprefix = 1;
                } else {
                    print STDERR "$exe NOT found in PATH, nor in libexec, using locate. Where is it?\n";
                }
            }
        }
        if (/KGlobal::dirs\(\)->saveLocation\(\s*\"([^\"]*)\"(,\s*[^\)]*)?\s*\)/) {
            my $suffix = $2;
            my ($loc, $add) = locationAndSubdir($1, "");
            #print STDERR "resource=$resource suffix=$suffix\n";
            if ($add ne "") {
                $add = " + $add";
            }
            if ($suffix) {
                $suffix =~ s/,\s*//;
                $add .= " + QLatin1Char('/') + $suffix)" unless $suffix eq "QString(" || $suffix eq "\"\"";
                #print STDERR "loc=$loc suffix=$suffix add=$add\n";
            }
            s/KGlobal::dirs\(\)->saveLocation\(.*\)/QStandardPaths::writableLocation($loc)$add/ if ($loc);
        }
        if (/KGlobal::dirs\(\)->resourceDirs\(\s*\"([^\"]*)\"\s*\)/) {
            my ($loc, $add) = locationAndSubdir($1, "");
            if ($add) {
                s/KGlobal::dirs\(\)->resourceDirs\(.*\)/QStandardPaths::locateAll($loc, $add, QStandardPaths::LocateDirectory)/;
            } elsif ($loc) {
                s/KGlobal::dirs\(\)->resourceDirs\(.*\)/QStandardPaths::standardLocations($loc) \/* WARNING: no more trailing slashes *\//;
            }
        }
        # While we're here...
        s/KGlobal::config\(\)/KSharedConfig::openConfig()/g;

        # ex: KSharedConfig::openConfig("mimeapps.list", KConfig::NoGlobals, "xdgdata-apps");
        if (my ($file, $flags, $resource) = ($_ =~ m/KSharedConfig::openConfig\(\s*([^,]*), ([^,]*), \s*\"([^\"]*)\"\s*\)/)) {
            my ($loc, $fileName) = locationAndSubdir($resource, $file);
            if ($loc) {
              s/KSharedConfig::openConfig\(.*\)/KSharedConfig::openConfig($fileName, $flags, $loc)/;
            }
        }
        if (/KStandardDirs::locateLocal\s*\(\s*\"tmp\"/) {
            s/KStandardDirs::locateLocal\s*\(\s*\"tmp\"\s*,/QDir::tempPath\(\) \+ QLatin1Char\(\'\/\'\) \+ /;
        }


    } $file;
    functionUtilkde::addIncludeInFile($file, "config-prefix.h") if ($addconfigprefix);
    if (not `grep -i dirs $file | grep -v kstandarddirs\.h`) {
      functionUtilkde::removeIncludeInFile($file, "kstandarddirs.h");
    }
    if (not `grep -i dirs $file | grep -v KStandardDirs`) {
      functionUtilkde::removeIncludeInFile($file, "KStandardDirs");
    }

    if (`grep QStandardPaths $file | grep -v '#include'`) {
      functionUtilkde::addIncludeInFile($file, "QStandardPaths");
    }
}

functionUtilkde::diffFile( "@ARGV" );
