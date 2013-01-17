#!/usr/bin/perl -w

# David Faure <faure@kde.org>
# KUrl -> QUrl + QUrlPathInfo

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    functionUtilkde::substInFile {
        if (/(\w*)\.adjustPath\(\s*KUrl::RemoveTrailingSlash\s*\)/) {
            my $urlvar = $1;
            s/$urlvar\./QUrlPathInfo::/;
            s/adjustPath\(\s*KUrl::RemoveTrailingSlash\s*/adjustPath\($urlvar, QUrlPathInfo::StripTrailingSlash/;
        }
    } $file;
}

functionUtilkde::diffFile( "@ARGV" );
