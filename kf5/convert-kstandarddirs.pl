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
    functionUtilkde::substInFile {
	s/KGlobal::dirs\(\)->locate/KStandardDirs::locate/;
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
    } $file;
}
functionUtilkde::diffFile( "@ARGV" );
