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
    my %varname = ();
    my $qmimedatabaseAdded;

    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        # see http://community.kde.org/Frameworks/Porting_Notes#KDECore_Changes
        if (/KMimeType::Ptr\s+(\w+)/) {
           my $var = $1;
           $varname{$var} = 1;
           s/KMimeType::Ptr/QMimeType/g;
        }
        if (/(\w+)\->is\s*\(/) {
           my $var = $1;
           if (defined $varname{$var}) {
              s/(\w+)\->is/$var\.inherits/;
           }
        }
        if (/if\s+\(\s*(\w+)\s*\)/) {
           my $var = $1;
           if (defined $varname{$var}) {
              s/if\s+\(\s*$var\s*\)/if \($var\.isValid\(\)\)/;
           }

        }
        if (/if\s*\(\s*(\w+).isNull\s*\(\s*\)/) {
           my $var = $1;
        
           if (defined $varname{$var}) {
              s/if\s*\(\s*$var.isNull\s*\(\s*\)/if \(!$var\.isValid\(\)/;
           }
        }
        if (/if\s*\(\s*!(\w+).isNull\s*\(\s*\)/) {
           my $var = $1;
        
           if (defined $varname{$var}) {
              s/if\s*\(\s*!$var.isNull\s*\(\s*\)/if \($var\.isValid\(\)/;
           }
        }

        if (/(\w+)\->isDefault\s*\(/) {
           my $var = $1;
           if (defined $varname{$var}) {
              s/(\w+)\->isDefault/$var\.isDefault/;
           }
        }


        my $regexpMimeTypeForName = qr/
           ^(\s*)                        # (1) Indentation
           (.*)                          # (2) before
           KMimeType::mimeType\s*\(
           (.*)$                         # (3) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $before, $afterreg) = $_ =~ $regexpMimeTypeForName) {
            $_ = $indent . "QMimeDatabase db;\n";
            $afterreg =~ s/,\s*KMimeType::ResolveAliases//;
            $_ .= $indent . $before . "db.mimeTypeForName(" . $afterreg . "\n";
            $qmimedatabaseAdded = 1;
        }
        if (/(\w+)\->iconName\s*\(/) {
           my $var = $1;
           if (defined $varname{$var}) {
              s/(\w+)\->iconName/$var\.iconName/;
           }
        }
        if (/(\w+)\->comment\s*\(/) {
           my $var = $1;
           if (defined $varname{$var}) {
              s/(\w+)\->comment/$var\.comment/;
           }
        }


 
        s/, KMimeType::DontResolveAlias//;
        if (/KMimeType::findByUrl\s*\(/) {
          s/KMimeType::findByUrl\s*\((.*),\s*0\s*,\s*true\s*\)/db.mimeTypeForFile($1.path(), QMimeDatabase::MatchExtension)/;
          s/KMimeType::findByUrl\s*\(/db.mimeTypeForUrl(/;
        }
        s/KMimeType::findByPath\s*\((.*),\s*0\s*,\s*true\s*\)/db.mimeTypeForFile($1, QMimeDatabase::MatchExtension)/;
        s/KMimeType::findByPath\s*\(/db.mimeTypeForFile(/;
        s/KMimeType::findByContent\s*\(/db.mimeTypeForData(/;
        s/KMimeType::findByNameAndContent\s*\(/db.mimeTypeForNameAndData(/;
        s/KMimeType::findByFileContent\s*\(\s*(\w+)\s*\)/db.mimeTypeForFile($1, QMimeDatabase::MatchContent)/;
        s/(\w+)->name() == KMimeType::defaultMimeType/$1.isDefault/;
        s/allParentMimeTypes/allAncestors/;
        s/->name/\.name/ if (/\.mimeTypeFor/);
        s/KMimeType::extractKnownExtension/db.suffixForFileName/;
        s/KMimeType::allMimeTypes/db.allMimeTypes/;

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        functionUtilkde::removeIncludeInFile($file, "KMimeType");
        functionUtilkde::removeIncludeInFile($file, "kmimetype.h");

        functionUtilkde::addIncludeInFile($file, "QMimeDatabase");
        functionUtilkde::addIncludeInFile($file, "QMimeType");
    }
}

functionUtilkde::diffFile( "@ARGV" );
