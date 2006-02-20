#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2006 GPL
# Script to use QT macros: Q_SLOTS/Q_SIGNALS
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
    my @necessaryIncludes = ();
    open(my $FILE, $file) or warn "We can't open file $file:$!\n";
    my @l = map {
	my $orig = $_;
	$_ =~ s/KURL /KUrl /g;
	$_ =~ s/KURL&/KUrl&/g;
	$_ =~ s/KURL\*/KUrl\*/g;
	$_ =~ s/KURL::/KUrl::/g;
	$_ =~ s/class KURL;/class KUrl;/;
	$_ =~ s/KURL\(/KUrl\(/g;
	$_ =~ s/KURL>/KUrl>/;
	$modified ||= $orig ne $_;
	$_;
    } <$FILE>;

    if ($modified) {
	open (my $OUT, ">$file");
	print $OUT @l;
    }

}
functionUtilkde::diffFile( <$F> );

