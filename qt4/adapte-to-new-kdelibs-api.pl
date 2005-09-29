#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function allows to adapt file to new kdelibs API

use lib qw( . );
use functionUtilkde; 

open(my $F, q(find -name "*" |));
my $file;
while ($file = <$F>) {
 	chomp;
	next if functionUtilkde::excludeFile( $file);

	my $modified;
		
	open(my $FILE, $file) or warn "We can't open file $$!\n";
	my @l = map {
		my $orig = $_;
		if (my ($prefix, $suite, $end) = /(.*)(addVBoxPage.*)\s*$/) {
	            my $changes = $prefix;
        	    $changes =~ s!Q3Frame!KVBox!;
	            $_ = $changes . $suite . $end . "\n";
		}	
		if (my ($prefix, $suite, $end) = /(.*)(addHBoxPage.*)\s*$/) {
        	    my $changes = $prefix;
	            $changes =~ s!Q3Frame!KHBox!;
        	    $_ = $changes . $suite . $end . "\n";
		}	
		if (my ($prefix, $suite, $end) = /(.*)(makeVBoxMainWidget.*)\s*$/) {
	            my $changes = $prefix;
        	    $changes =~ s!Q3Frame!KVBox!;
	            $_ = $changes . $suite . $end . "\n";
		}	
		if (my ($prefix, $suite, $end) = /(.*)(makeHBoxMainWidget.*)\s*$/) {
        	    my $changes = $prefix;
	            $changes =~ s!Q3Frame!KHBox!;
        	    $_ = $changes . $suite . $end . "\n";
		}	
		s!#include <kaccelmanager.h>!#include <kacceleratormanager.h>!;
		s!KStringHandler::matchFilename!KStringHandler::matchFileName!;
		if ( $_ =~ /KApplication::random/ ) {
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
		s!kdatetbl.h!kdatetable.h!;
		#TODO test it, perhaps remove all before isRestored (for example if( kapp-> isRestored())
		s!kapp->isRestored!QApplication::isSessionRestored!;
	    	$modified ||= $orig ne $_;
		    $_;
		} <$FILE>;

	if ($modified) {
	    open (my $OUT, ">$file");
	    print $OUT @l;
	}
}
functionUtilkde::diffFile( <$F> );
