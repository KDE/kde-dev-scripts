#!/usr/bin/perl

# David Faure <faure@kde.org> 2007
# Based on script by Laurent Montel <montel@kde.org> 2005
# This script converts KMimeType::pixmap (deprecated) to KIconLoader::loadMimeTypeIcon

use lib qw( . );
use functionUtilkde;

foreach my $file (@ARGV) {
    local *F;
    open F, "+<", $file or do { print STDOUT "open($file) failled : \"$!\"\n"; next };
    my $str = join '', <F>;
    $str =~ s/(KMimeType::mimeType\([^)]*\))->pixmap\(([^)]*)\)/KIconLoader::global()->loadMimeTypeIcon($1->iconName(),$2)/sg;
    $str =~ s/(\w*)->pixmap\(([^)]*)\)/KIconLoader::global()->loadMimeTypeIcon($1->iconName(),$2)/sg;
    seek F, 0, 0;
    print F $str;
    truncate F, tell(F);
    close F;
}

functionUtilkde::diffFile( "@ARGV" );

