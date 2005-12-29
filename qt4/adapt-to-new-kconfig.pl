#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2005 GPL
# This function allows to adapt file to new kconfig  API

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

sub convertFunction
{
    my ($convertName, $type, $defaultValue) = @_;
    my $result;
    if (my ($prefix, $parenthese, $end) = /(.*$convertName\s*\()(.*?)(\).*)/) {
	#warn "prefix !!!! :<$prefix>  parenthese <$parenthese> end <$end>\n";	
	my $changes = $end;
	$changes =~ s/\)//;
	if ( my ($firstelement, $secondelement) = $parenthese =~ m!(.*),\s*(.*)\)$!) {
	    #warn "firstelement <$firstelement>  secondelement <$secondelement>\n";
	    $result = $prefix . $firstelement . ", QVariant(" . $secondelement . ")).$type" . $changes ."\n";
	    
	}
	elsif ( my ($firstelement, $secondelement) = $parenthese =~ m!(.*),\s*(.*)$!) { #just one parenthese
	    #warn "firstelement <$firstelement>  secondelement <$secondelement>\n";
	    $result = $prefix . $firstelement . ", QVariant(" . $secondelement . ")).$type" . $changes ."\n";
	}
	else #just one argument default argument is false
	{
	    #warn "1 argument : <$parenthese> \n";
	    $result = $prefix . $parenthese . ", QVariant($defaultValue)"  . ").$type" . $changes ."\n";
	}
	$result =~ s!$convertName!readEntry!;
    }
    return $result;
}

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
	my $newValue = convertFunction("readBoolEntry","toBool()","false");
	if( $newValue ) {
	    $_ = $newValue;
	}
	$newValue = convertFunction("readNumEntry","toInt()","0");
	if( $newValue ) {
	    $_ = $newValue;
	}
    $newValue = convertFunction("readDoubleNumEntry","toDouble()","0.0");
    if( $newValue ) {
        $_ = $newValue;
    }
    $newValue = convertFunction("readUnsignedNumEntry","toUInt()","0");
    if( $newValue ) {
        $_ = $newValue;
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
