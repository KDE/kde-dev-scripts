#!/usr/bin/perl 

# David Faure <faure@kde.org>
# KKeyDialog -> KShortcutDialog

use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
	s!kkeydialog\.h!kshortcutsdialog.h!g;
	s!KKeyDialog!KShortcutsDialog!g;
	s!KKeyChooser!KShortcutsEditor!g;
    } $file;
}
functionUtilkde::diffFile( "@ARGV" );
