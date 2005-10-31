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
				
				#create tab for function entry 
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
                $convertStruct[$value] = "$firstelement\->readBoolEntry";
                $value++;
                $convertStruct[$value] = "$variable\.readBoolEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readFontEntry";
                $value++;
                $convertStruct[$value] = "$variable\.readFontEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readRectEntry";
                $value++;
                $convertStruct[$value] = "$variable\.readRectEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readPointEntry";
                $value++;
                $convertStruct[$value] = "$variable\.readPointEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readListEntry";
                $value++;
                $convertStruct[$value] = "$variable\.readListEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readPathEntry";
                $value++;
                $convertStruct[$value] = "$variable\.readPathEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readPathListEntry";
                $value++;
                $convertStruct[$value] = "$variable\.readPathListEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readNumEntry";
                $value++;
                $convertStruct[$value] = "$variable\.readNumEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readSizeEntry";
                $value++;
                $convertStruct[$value] = "$variable\.readSizeEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readColorEntry";
                $value++;
                $convertStruct[$value] = "$variable\.readColorEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readDoubleNumEntry";
                $value++;
                $convertStruct[$value] = "$variable\.readDoubleNumEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readUnsignedNum64Entry";
                $value++;
                $convertStruct[$value] = "$variable\.readUnsignedNum64Entry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readLongNumEntry";
                $value++;
                $convertStruct[$value] = "$variable\.readLongNumEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readDateTimeEntry";
                $value++;
                $convertStruct[$value] = "$variable\.readDateTimeEntry";
                $value++;
                $convertStruct[$value] = "$firstelement\->readEntryUntranslated";
                $value++;
                $convertStruct[$value] = "$variable\.readEntryUntranslated";
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
