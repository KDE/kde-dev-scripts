#!/usr/bin/perl

# Laurent Montel <montel@kde.org> 2005
# Thanks to Rafael Garcia-Suarez and Thierry Vignaud for his help
# This script try to fix error when we didn't add setVersion after a QDataStream

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;

foreach my $file (@ARGV) {
    local *F;
	open F, "+<", $file or do { print STDOUT "open($file) failled : \"$!\"\n"; next };
    my $str = join '', <F>;
    $str =~ s/(\s*)(QDataStream (\w*)\s*\(\s*\&\w*\s*,\s*[a-zA-Z0-9_:]+\s*\);\n)(?!\s*\3\.setVersion)/$1$2$1$3.setVersion(QDataStream::Qt_3_1);\n/sg;
    seek F, 0, 0;
    print F $str;
    truncate F, tell(F);
    close F;
}

functionUtilkde::diffFile( "@ARGV" );

