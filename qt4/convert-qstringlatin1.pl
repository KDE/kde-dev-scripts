#!/usr/bin/perl

# laurent Montel <montel@kde.org>
# This function convert QLatin1String instead of QString::fromLatin1()
# see "qlatin1string.html" "is faster than converting the Latin-1 strings using QString::fromLatin1()." 
# TODO: test "QString::fromLatin1(blabla())"
#
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {
    functionUtilkde::substInFile { 
			if( my ($prefix, $suite, $end) = /(.*)(QString::fromLatin1.*?\))(.*$)/) {
				s!QString::fromLatin1!QLatin1String! if $end !~ /(\.arg|\.mid|\.prepend|\.append|\.toLower|\.upper)/;
			}
    } $file;
}
functionUtilkde::diffFile( "@ARGV" );

