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

        # see https://community.kde.org/Frameworks/Porting_Notes#KDECore_Changes
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
        if (/(\w+)\->patterns\s*\(/) {
           my $var = $1;
           if (defined $varname{$var}) {
              s/(\w+)\->patterns/$var\.globPatterns/;
           }
        }


          
        s/, KMimeType::DontResolveAlias//;
        my $regexFindByUrlArgs = qr/
           ^(\s*)                        # (1) Indentation
           (.*)                          # (2) before
           KMimeType::findByUrl\s*\((.*),\s*0\s*,\s*true\s*\) # (3) argument
           (.*)$                         # (3) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $before, $args, $afterreg) = $_ =~ $regexFindByUrlArgs) {
            my $addDataBase = "";
            if (not defined $qmimedatabaseAdded) {
               $addDataBase = $indent . "QMimeDatabase db;\n";
            }
            $_ = $addDataBase . $indent . "db.mimeTypeForFile($args.path(), QMimeDatabase::MatchExtension)" . "$afterreg" . "\n";
        }



        if (/KMimeType::findByUrl\s*\(/) {
          if (not defined $qmimedatabaseAdded) {
              $_ =  "QMimeDatabase db;\n" . $_;
          }
          s/KMimeType::findByUrl\s*\(/db.mimeTypeForUrl(/;
        }
        #KMimeType::Ptr mime = KMimeType::findByContent( body );
        my $regexFindByContent = qr/
            ^(\s*)                        # (1) Indentation
            (.*)\s+                          # (2) before
            (\w+)\s*=\s*                  # (3) variable
            KMimeType::findByContent\s*\(
           (.*)$                          # (4) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $before, $variable, $afterreg) = $_ =~ $regexFindByContent) {
            $varname{$variable} = 1;
            warn "variable $variable before :$before $_\n";
            my $addDataBase;
            if (not defined $qmimedatabaseAdded) {
               $addDataBase = $indent . "QMimeDatabase db;\n";
            }
            if (defined $addDataBase ) {
               $_ = $addDataBase . $_;
            } 
            s/KMimeType::findByContent\s*\(/db.mimeTypeForData(/;
        }

        s/KMimeType::findByPath\s*\((.*),\s*0\s*,\s*true\s*\)/db.mimeTypeForFile($1, QMimeDatabase::MatchExtension)/;
        s/KMimeType::findByPath\s*\(/db.mimeTypeForFile(/;
        s/KMimeType::findByNameAndContent\s*\(/db.mimeTypeForFileNameAndData(/;
        s/KMimeType::findByFileContent\s*\(\s*(\w+)\s*\)/db.mimeTypeForFile($1, QMimeDatabase::MatchContent)/;
        s/(\w+)->name() == KMimeType::defaultMimeType/$1.isDefault/;
        s/allParentMimeTypes/allAncestors/;
        s/->name/\.name/ if (/\.mimeTypeFor/);
        s/KMimeType::extractKnownExtension/db.suffixForFileName/;
        s/KMimeType::allMimeTypes/db.allMimeTypes/;
        if (/(\w+)->name\s*\(\)/) {
           my $var = $1;
           if (defined $varname{$var}) {
              s/(\w+)\->name/$var\.name/;
           }
        }

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
