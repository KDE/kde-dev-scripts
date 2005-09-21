#!/usr/bin/perl 

# Laurent Montel <montel@kde.org>
# Convert Q3CString to QByteArray or QString or DCOPCString

use lib qw( . );
use functionUtilkde; 

foreach my $file (@ARGV) {
	functionUtilkde::substInFile {
			my $value = $_;
			if ( $value =~ /registerAs/ ) {
					s!Q3CString!DCOPCString!g;
			}
			elsif ( $value !~ /sprintf/ ) {
					s!Q3CString!QByteArray!g;
			}
			else {
					s!Q3CString!QString!g;
			}
	} $file;
}
functionUtilkde::diffFile( "@ARGV" );

