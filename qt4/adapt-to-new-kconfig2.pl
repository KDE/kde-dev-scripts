#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2006 GPL
# This function allows to adapt file to new kconfig  API

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

open(my $F, q(find -name "*" |));
my $file;
my $warning;
while ($file = <$F>) {
    chomp $file;
    next if functionUtilkde::excludeFile( $file);

    my $modified;
    my @necessaryIncludes = ();
    open(my $FILE, $file) or warn "We can't open file $file:$!\n";
    my @l = map {
	my $orig = $_;
	if (my ($prefix, $parenthese, $end) = /(.*writeEntry\s*\()(.*)(\).*)/) {
	    warn "prefix !!!! :<$prefix>  parenthese <$parenthese> end <$end>\n";	
	    my $changes = $end;
	    $changes =~ s/\)//;
	    if ( my ($firstelement, $secondelement,$thirdelement,$fouthelement) = $parenthese =~ m!(.*),\s*(.*),\s*(.*),\s*(.*)$!) {
		warn "4 elements : firstelement <$firstelement>  secondelement <$secondelement> thirdelement<$thirdelement> fouthelement <$fouthelement> \n";
		if ( $thirdelement =~ /(true|TRUE)/ && $fouthelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Persistent|KConfigBase::Global" .  $end . "\n";
	        }
		elsif ( $thirdelement =~ /(false|FALSE)/ && $fouthelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Global" .  $end . "\n";
		}
		elsif ( $thirdelement =~ /(true|TRUE)/ && $fouthelement =~ /(false|FALSE)/ )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Persistent" .  $end . "\n";
		}
		
	    }
	    elsif ( my ($firstelement, $secondelement,$thirdelement) = $parenthese =~ m!(.*),\s*(.*),\s*(.*)$!) {
		warn " 3 elements :  firstelement <$firstelement>  secondelement <$secondelement> thirdelement<$thirdelement>\n";
		if ( $thirdelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Persistent" .  $end . "\n";
		}
	    }
	}	
	$modified ||= $orig ne $_;
	$_;
    } <$FILE>;

    if ($modified) {
	open (my $OUT, ">$file");
	print $OUT @l;
    }

}
functionUtilkde::diffFile( <$F> );
warn "Warning: $warning\n";
