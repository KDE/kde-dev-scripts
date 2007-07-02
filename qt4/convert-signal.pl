#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2006 GPL
# Script to use Qt macros: Q_SLOTS/Q_SIGNALS
# Becarefull use this script on lib not on all files
# 
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
    open(my $FILE, $file) or warn "We can't open file $file:$!\n";
    my @l = map {
	my $orig = $_;

	if ( $_ !~ /k_dcop_signals:/ ) {
		$_ =~ s/signals:/Q_SIGNALS:/;
	}
	$_ =~ s/slots:/Q_SLOTS:/;
	$modified ||= $orig ne $_;
	$_;
    } <$FILE>;

    if ($modified) {
	open (my $OUT, ">$file");
	print $OUT @l;
    }

}
functionUtilkde::diffFile( <$F> );

