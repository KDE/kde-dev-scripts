#!/usr/bin/perl

# Laurent Montel <montel@kde.org> 2007 GPL
# This script ports everything in the current directory (and recursively) to the Qt4 API,
# for things that can be done automatically.
# This script must be launch after that kdDebug was convert to kDebug etc.

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

open(my $F, q(find -name "*" |));
my $file;
my $warning;
my @files = ();
while ($file = <$F>) {
    chomp $file;
    next if functionUtilkde::excludeFile( $file);
    if (functionUtilkde::substInFile {

	if( $_ =~ /kDebug/ ) {
		s!\s*\<\<\s*endl\s*;!;!;
	}
        if( $_ =~ /kWarning/ ) {
		s!\s*\<\<\s*endl\s*;!;!;
        }
        if( $_ =~ /kFatal/ ) {
		s!\s*\<\<\s*endl\s*;!;!;
        }

    } $file) { push(@files,$file); }
}
functionUtilkde::diffFile( @files );
warn "Warning: $warning\n" if ($warning != "");
