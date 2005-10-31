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
	my @convertStruct;
	my $value = 0;
	open(my $FILE, $file) or warn "We can't open file $$!\n";
	my @l = map {
		my $orig = $_;
		if (my ($blank, $prefix, $contenu) = m!^(\s*.*)(KConfigGroupSaver.*)\((.*)\s*\);$!) {
			my $variable = $prefix;
			$variable =~ s!KConfigGroupSaver!!;
			$variable =~ s! !!g;
			$variable =~ s!saver!group!;

			if( my ($firstelement, $secondelement) = m!.*?\(\s*(.*),\s*(.*)\);\s*$!) {
				$_ =~ s!KConfigGroupSaver!KConfigGroup!g;
				$_ =~ s!saver!group!;
				$convertStruct[$value] = "$firstelement\->readEntry";
				$value++; 
				$convertStruct[$value] = "$variable\.readEntry";
				$value++;
                $convertStruct[$value] = "$firstelement\->writeEntry";
                $value++;
                $convertStruct[$value] = "$variable\.writeEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->deleteEntry";
                $value++;
                $convertStruct[$value] = "$variable\.deleteEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->sync";
                $value++;
                $convertStruct[$value] = "$variable\.sync";
                $value++;
			}
			
		}
	    	$modified ||= $orig ne $_;
		    $_;
	    } <$FILE>;

	if ($modified) {
	    open (my $OUT, ">$file");
	    print $OUT @l;
	}
	if ($modified )
	{
   		local *F;
   		open F, "+<", $file or do { print STDOUT "open($file) failled : \"$!\"\n"; next };
   		my $str = join '', <F>;
		my $compte=0;
		while ($compte < $value) 
		{
			warn "compte: $compte \n";
			my $search = $convertStruct[$compte];
			my $replace = $convertStruct[$compte+1];
			$str =~ s!$search!$replace!g;
			
			$compte = $compte+ 2;
			
			seek F, 0, 0;
			print F $str;
			truncate F, tell(F);
		}
		close F;
	}
}
functionUtilkde::diffFile( <$F> );
