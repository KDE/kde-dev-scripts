#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function convert QLatin1String instead of QString::fromLatin1()
# see "qlatin1string.html" "is faster than converting the Latin-1 strings using QString::fromLatin1()." 
# TODO: test "QString::fromLatin1(blabla())"
# 
use lib qw( . );
use functionUtilkde; 

foreach my $file (@ARGV) {
    functionUtilkde::substInFile {
			if( my ($prefix, $suite, $end) = /(.*)(QString::fromLatin1.*?\))(.*$)/) {
			#warn "SUITE contains «$suite»\nEND contains «$end»";
			s!QString::fromLatin1!QLatin1String! if $end !~ /(\.arg|\.mid|\.prepend|\.append)/;
			#warn "element : $prefix : suite : $suite : end : $end ::::\n";
					
			}
    } $file;
}
functionUtilkde::diffFile( "@ARGV" );

