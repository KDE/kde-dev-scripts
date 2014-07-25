#!/usr/bin/perl -w

# David Faure <faure@kde.org>
# KMimeType -> QMimeType
# find -iname "*.cpp"|xargs kde-dev-scripts/kf5/convert-kmimetype.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        # see http://community.kde.org/Frameworks/Porting_Notes#KDECore_Changes
        s/KMimeType::Ptr/QMimeType/g;
        s/if \(mime\)/if (mime.isValid())/;
        s/if\s*\(mime.isNull\(\)\)/if (!mime.isValid())/;
        s/KMimeType::mimeType\s*\(/db.mimeTypeForName(/;
        s/, KMimeType::DontResolveAlias//;
        s/KMimeType::findByUrl\s*\((.*),\s*0,\s*true\s*\)/db.mimeTypeForFile($1.path(), QMimeDatabase::MatchExtension)/;
        s/KMimeType::findByUrl\s*\(/db.mimeTypeForUrl(/;
        s/KMimeType::findByPath\s*\((.*),\s*0,\s*true\s*\)/db.mimeTypeForFile($1, QMimeDatabase::MatchExtension)/;
        s/KMimeType::findByPath\s*\(/db.mimeTypeForFile(/;
        s/KMimeType::findByContent\s*\(/db.mimeTypeForData(/;
        s/KMimeType::findByNameAndContent\s*\(/db.mimeTypeForNameAndData(/;
        s/KMimeType::findByFileContent\s*\(\s*(\w+)\s*\)/db.mimeTypeForFile($1, QMimeDatabase::MatchContent)/;
        s/(\w+)->name() == KMimeType::defaultMimeType/$1.isDefault/;
        s/allParentMimeTypes/allAncestors/;
        s/->name/\.name/ if (/\.mimeTypeFor/);
        s/KMimeType::extractKnownExtension/db.suffixForFileName/;
        s/KMimeType::allMimeTypes/db.allMimeTypes/;

        if (/#include <KMimeType>/ || /#include <kmimetype\.h>/) {
            $_ = "#include <QMimeType>\n#include <QMimeDatabase>\n";
        }

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
    }
}

functionUtilkde::diffFile( "@ARGV" );
