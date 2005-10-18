#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function allows to adapt file to new kdelibs API

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;


open(my $F, q(find -name "*" |));
my $file;
while ($file = <$F>) {
    chomp $file;
	next if functionUtilkde::excludeFile( $file);

	my $modified;
	my $necessaryToAddInclude;	
	open(my $FILE, $file) or warn "We can't open file $$!\n";
	my @l = map {
		my $orig = $_;
	    	$modified ||= $orig ne $_;
		    $_;
	    } <$FILE>;

	if ($modified) {
	    open (my $OUT, ">$file");
	    print $OUT @l;
	}
	if($necessaryToAddInclude)
	{
	    functionUtilkde::addIncludeInFile( $file, "krandom.h");
	}
}
functionUtilkde::diffFile( <$F> );
