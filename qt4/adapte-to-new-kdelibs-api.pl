#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function allows to adapt file to new kdelibs API

use lib qw( . );
use functionUtilkde; 


foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
    }
	s!#include <kaccelmanager.h>!#include <kacceleratormanager.h>!;
	s!KStringHandler::matchFilename!KStringHandler::matchFileName!;
	if ( $_ ~ = /KApplication::random/ ) {
			s!KApplication::random!KRandom::random!;
			functionUtilkde::addIncludeInFile( $file, "krandom.h");
		}
	s!KFindDialog::WholeWordsOnly!KFind::WholeWordsOnly!;
	s!KFindDialog::FromCursor!KFind::FromCursor!;
	s!KFindDialog::SelectedText!KFind::SelectedText!;
	s!KFindDialog::CaseSensitive!KFind::CaseSensitive!;
	s!KFindDialog::FindBackwards!KFind::FindBackwards!;
	s!KFindDialog::RegularExpression!KFind::RegularExpression!;
	s!KFindDialog::FindIncremental!KFind::FindIncremental!;
	s!KFindDialog::MinimumUserOption!KFind::MinimumUserOption!;
	} $file;
}
functionUtilkde::diffFile( "@ARGV" );

