#!/usr/bin/perl -w

# David Faure <faure@kde.org>
# KUrl -> QUrl
# Usage: convert-kurl.pl *.h *.cpp

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;
my %urls = (); # shared between all files

foreach my $file (@ARGV) {

    # I don't use functionUtilkde::substInFile because it touches all files, even those which were not modified.
    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        s/#include <kurl.h>/#include <QUrl>/;
        s/#include <KUrl>/#include <QUrl>/;
        s/class KUrl\;/class QUrl\;/;
        s/KUrl\(\)\;/QUrl()\;/;
        s/KUrl::toPercentEncoding\b/QUrl::toPercentEncoding/;
        s/KUrl::fromPercentEncoding\b/QUrl::fromPercentEncoding/;
        s,KUrl::fromPath\b,QUrl::fromLocalFile,;

        # Detect variables being declared as KUrl
        if (/const KUrl\s*&\s*(\w+)/ || /^\s*KUrl\s+(\w+)\s*[=\;]/ || /^\s*KUrl\s+(\w+)\s*\(/) {
            $urls{$1} = 1;
            print STDERR "found KUrl var: $1\n";
            s/const KUrl\s*&\s*/const QUrl &/;
            s/KUrl (\w+)\;/QUrl $1\;/;
            s/KUrl (\w+)\s*=/QUrl $1 =/;
        }
        # Detect variables being declared as QUrl (for copying into KUrls, below)
        if (/const QUrl\s*&\s*(\w+)/ || /^\s*QUrl\s+(\w+)\s*(?:=|;)/ || /^\s*QUrl\s+(\w+)\s*\(/) {
            #print STDERR "found QUrl var: $1\n";
            $urls{$1} = 2;
        }

        # Do not port KUrl(QString) automatically, that's impossible (need to find out if the string is a path or a URL...)
        # Except when it's clear :)
        if (my ($var, $value) = /KUrl (\w+)\s*\(\s*(\"[^\"]*\")\s*\)/) {
            if ($value =~ /:\//) { # full URL -> clear
                s/KUrl $var/QUrl $var/;
            }
        }
        if (my ($value) = /KUrl\s*\(\s*(\"[^\"]*\")\s*\)/) { # anonymous var
            if ($value =~ /:\//) { # full URL -> clear
                s/KUrl\s*\(\s*\Q$value\E\s*\)/QUrl($value)/;
            }
        }
        if (my ($var, $value) = /KUrl (\w+)\s*\(\s*(\w+)\s*\)/) {
            if (defined $urls{$value}) { # copy of another URL -> clear
                s/KUrl $var/QUrl $var/;
            }
        }
        if (/(\w+)\.pass\(\)/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.pass\(\)/$url\.password()/g;
        }
        if (/(\w+)\.setPass\b/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.setPass\b/$url\.setPassword/g;
        }

        if (/(\w+)\.protocol\(\)/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.protocol\(\)/$url\.scheme()/g;
        }
        if (/(\w+)\.setProtocol\(/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.setProtocol\(/$url\.setScheme(/g;
        }
        if (/(\w+)\.user\(\)/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.user\(\)/$url\.userName()/g;
        }
        if (/(\w+)\.setUser\b/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.setUser\b/$url\.setUserName/g;
        }
        if (/(\w+)\.setRef\b/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.setRef\b/$url\.setFragment/g;
        }
        if (/(\w+)\.htmlRef\(\)/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.htmlRef\(\)/$url\.fragment(QUrl::FullyDecoded)/g;
        }
        if (/(\w+)\.hasHTMLRef\(\)/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.hasHTMLRef\(\)/$url\.hasFragment()/g;
        }

        if (/(\w+)\.hasRef\(\)/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.hasRef\(\)/$url\.hasFragment()/g;
        }

        # url.adjustPath(KUrl::RemoveTrailingSlash) => url = url.adjusted(QUrl::StripTrailingSlash);
        if (/(\w*).adjustPath\(\s*KUrl::RemoveTrailingSlash\s*\)/) {
            my $urlvar = $1;
            s/adjustPath\(\s*KUrl::RemoveTrailingSlash\s*/$urlvar = $urlvar\.adjusted(QUrl::StripTrailingSlash)/;
        }
        # url.directory()
        if (/(\w+)\.directory\(\)/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.directory\(\)/$url\.adjusted(QUrl::RemoveFilename|QUrl::StripTrailingSlash).path()/g;
        }
        # url.path(KUrl::RemoveTrailingSlash)
        if (/(\w+)\.path\(KUrl::RemoveTrailingSlash\)/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.path\(KUrl::RemoveTrailingSlash\)/$url\.adjusted(QUrl::StripTrailingSlash).path()/g;
        }
        # url.toLocalFile(KUrl::RemoveTrailingSlash)
        if (/(\w+)\.toLocalFile\(KUrl::RemoveTrailingSlash\)/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.toLocalFile\(KUrl::RemoveTrailingSlash\)/$url\.adjusted(QUrl::StripTrailingSlash).toLocalFile()/g;
        }
        # url.adjustPath(KUrl::RemoveTrailingSlash)
        if (/(\w+)\.adjustPath\(KUrl::RemoveTrailingSlash\)/ && defined $urls{$1}) {
            my $url = $1;
            s/$url\.adjustPath\(KUrl::RemoveTrailingSlash\)/$url = $url\.adjusted(QUrl::StripTrailingSlash)/g;
        }
        # url.addPath(path)
        if (my ($indent, $url, $path) = /^(\s*)(\w+)\.addPath\(\s*([^\)]*)\s*\)/) {
            if (defined $urls{$url}) {
                s/$url\.addPath\(\s*$path\s*\)\;/$url = $url\.adjusted(QUrl::StripTrailingSlash)\;\n$indent$url.setPath($url.path() + '\/' + $path)\;/;
            }
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
