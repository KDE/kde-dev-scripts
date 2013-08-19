#!/usr/bin/perl -w

# David Faure <faure@kde.org>
# KUrl -> QUrl
# QUrlPathInfo -> QUrl
#    (QUrlPathInfo was a temporary API in kdelibs-frameworks, before QUrl was extended in Qt 5.2)

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $infoVar;
    my $urlVar;

    # I don't use functionUtilkde::substInFile because it touches all files, even those which were not modified.
    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        # kdelibs: QUrlPathInfo pathInfo(url);
        if (/QUrlPathInfo (\w*)\(\s*([\.\-\>_\w]*)\s*\)/) {
            # Record info->url association
            $infoVar = $1;
            $urlVar = $2;
            #print STDERR "infoVar=$infoVar urlVar=$urlVar\n";
        } elsif (/^}/) {  # end of method
            undef $infoVar;
        }
        # kdelibs: pathInfo.adjustPath(QUrlPathInfo::StripTrailingSlash);
        #    => url = url.adjusted(QUrl::StripTrailingSlash);
        if (defined $infoVar && /$infoVar\.adjustPath\(\s*QUrlPathInfo::StripTrailingSlash\s*\)/) {
            s/$infoVar.adjustPath\(\s*QUrlPathInfo::StripTrailingSlash\s*\)/$urlVar = $urlVar\.adjusted(QUrl::StripTrailingSlash)/;
        }
        # kdelibs: QUrlPathInfo::adjustPath(url, QUrlPathInfo::StripTrailingSlash);
        #    => url = url.adjusted(QUrl::StripTrailingSlash);
        s/QUrlPathInfo::adjustPath\(\s*(\w*)\s*,\s*QUrlPathInfo::StripTrailingSlash\s*\)/$1 = $1\.adjusted(QUrl::StripTrailingSlash)/;
        # KDE4 code: url.adjustPath(KUrl::RemoveTrailingSlash);
        #     => url = url.adjusted(QUrl::StripTrailingSlash);
        if (/(\w*).adjustPath\(\s*KUrl::RemoveTrailingSlash\s*\)/) {
            my $urlvar = $1;
            s/adjustPath\(\s*KUrl::RemoveTrailingSlash\s*/$urlvar = $urlvar\.adjusted(QUrl::StripTrailingSlash)/;
        }

        # kdelibs: QUrlPathInfo(url1).url(QUrlPathInfo::StripTrailingSlash);
        s/QUrlPathInfo\(\s*([\.\-\>_\w]*)\s*\)\.url\(QUrlPathInfo::StripTrailingSlash\)/$1\.adjusted(QUrl::StripTrailingSlash)/g;

        # kdelibs: QUrlPathInfo(url).directory()
        s/QUrlPathInfo\(\s*([\.\-\>_\w]*)\s*\)\.directory\(\)/$1\.adjusted(QUrl::RemoveFilename|QUrl::StripTrailingSlash).path()/g;
        # kdelibs: QUrlPathInfo(url).fileName()
        s/QUrlPathInfo\(\s*([\.\-\>_\w]*)\s*\)\.fileName\(\)/$1\.fileName()/g;
        # kdelibs: QUrlPathInfo(url).directoryUrl() -> url.adjusted(QUrl::RemoveFilename|QUrl::StripTrailingSlash)
        s/QUrlPathInfo\(\s*([\.\-\>_\w]*)\s*\)\.directoryUrl\(\)/$1\.adjusted(QUrl::RemoveFilename|QUrl::StripTrailingSlash)/g;

        # pathInfo.fileName()
        if (defined $infoVar && /$infoVar\.fileName\(\)/) {
            s/$infoVar\.fileName\(\)/$urlVar\.fileName()/g;
        }
        # pathInfo.directory()
        if (defined $infoVar && /$infoVar\.directory\(\)/) {
            s/$infoVar\.directory\(\)/$urlVar\.adjusted(QUrl::RemoveFilename|QUrl::StripTrailingSlash).path()/g;
        }
        # pathInfo.localPath(QUrlPathInfo::StripTrailingSlash)
        if (defined $infoVar && /$infoVar\.localPath\(QUrlPathInfo::StripTrailingSlash\)/) {
            s/$infoVar\.localPath\(QUrlPathInfo::StripTrailingSlash\)/$urlVar\.adjusted(QUrl::StripTrailingSlash).toLocalFile()/g;
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
