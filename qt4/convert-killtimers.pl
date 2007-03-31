#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function convert killTimers to qt4 function

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
open(my $F, q(find -name "*" |));
my $file;
while ($file = <$F>) {
 	chomp $file;
	next if functionUtilkde::excludeFile( $file);

	my $modified;
		
	open(my $FILE, $file) or warn "We can't open file $$!\n";
	my @l = map {
	    my $orig = $_;
    
		#TODO fix for killTimers with arguments
		s!killTimers\s*\(\s*\);!QAbstractEventDispatcher::instance()->unregisterTimers(this);!;
		$modified ||= $orig ne $_;
	    $_;
	} <$FILE>;
	
	if ($modified) {
		my $OUT;
	    open ($OUT, ">$file");
	    print $OUT @l;
		close ($OUT);
		# necessary to gave complete url
		my $newUrl = $ENV{PWD} . "/" . $file;
		$newUrl =~ s!\./!!;
		functionUtilkde::addIncludeInFile( $newUrl, "QAbstractEventDispatcher");
	}
}
functionUtilkde::diffFile( <$F> );

