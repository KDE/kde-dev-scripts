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
	my %convertHash = ();
	open(my $FILE, $file) or warn "We can't open file $$!\n";
	my @l = map {
		my $orig = $_;
		if (my ($blank, $variable, $contenu) = m!^(\s*.*)(KConfigGroupSaver.*?\()(.*)\s*\);$!) {
			#warn "initial value for variable: '$variable'\n";
			$variable =~ s!KConfigGroupSaver!!;
			$variable =~ s! !!g;
			$variable =~ s!\(!!;
			my $saver = $variable;
			$variable =~ s!groupSaver!configGroup!;
			$variable =~ s!saver!group!;
			$variable =~ s!Saver!Group!;
			$variable =~ s!cgs!configGroup!;

			if( my ($firstelement, $groupname) = m!.*?\(\s*(.*),\s*(.*)\);\s*$!) {
				$_ =~ s!KConfigGroupSaver!KConfigGroup!g;
				$_ =~ s!$saver!$variable!;

				# Two cases:
				# Value-based: KConfigGroupSaver(&config) and config.readEntry
				# Pointer-based: KConfigGroupSaver(config) and config->readEntry
				my $valueBased = 0;
				$valueBased = 1 if ( $firstelement =~ s!^\&!! );

				my $config = ($valueBased) ? "$firstelement\." : "$firstelement\->";
				my $group = "$variable\.";

				print "variable='$variable firstelement=$firstelement groupname=$groupname valueBased=$valueBased group=$group'\n";

				#create tab for function entry
				$convertHash{$config.'readEntry'} = $group.'readEntry';
				$convertHash{$config.'writeEntry'} = $group.'writeEntry';
				$convertHash{$config.'writePathEntry'} = $group.'writePathEntry';
				$convertHash{$config.'deleteEntry'} = $group.'deleteEntry';
				$convertHash{$config.'readBoolEntry'} = $group.'readBoolEntry';
				$convertHash{$config.'readFontEntry'} = $group.'readFontEntry';
				$convertHash{$config.'readRectEntry'} = $group.'readRectEntry';
				$convertHash{$config.'readPointEntry'} = $group.'readPointEntry';
				$convertHash{$config.'readListEntry'} = $group.'readListEntry';
				$convertHash{$config.'readPathEntry'} = $group.'readPathEntry';
				$convertHash{$config.'readPathListEntry'} = $group.'readPathListEntry';
				$convertHash{$config.'readNumEntry'} = $group.'readNumEntry';
				$convertHash{$config.'readSizeEntry'} = $group.'readSizeEntry';
				$convertHash{$config.'readColorEntry'} = $group.'readColorEntry';
				$convertHash{$config.'readDoubleNumEntry'} = $group.'readDoubleNumEntry';
				$convertHash{$config.'readUnsignedNum64Entry'} = $group.'readUnsignedNum64Entry';
				$convertHash{$config.'readLongNumEntry'} = $group.'readLongNumEntry';
				$convertHash{$config.'readDateTimeEntry'} = $group.'readDateTimeEntry';
				$convertHash{$config.'readEntryUntranslated'} = $group.'readEntryUntranslated';
				$convertHash{$config.'entryIsImmutable'} = $group.'entryIsImmutable';
			    $convertHash{$config.'readUnsignedNumEntry'} = $group.'readUnsignedNumEntry';
				$convertHash{$config.'hasKey'} = $group.'hasKey';
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
   		open F, "+<", $file or do { print STDOUT "open($file) failed : \"$!\"\n"; next };
   		my $str = join '', <F>;

		while ( my ($search, $replace) = each(%convertHash) )
		{
			#warn "$search -> $replace\n";	
			$str =~ s!$search!$replace!g;
		}
		seek F, 0, 0;
		print F $str;
		truncate F, tell(F);
		close F;
	}
}
functionUtilkde::diffFile( <$F> );
