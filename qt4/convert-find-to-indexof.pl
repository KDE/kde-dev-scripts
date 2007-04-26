#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2006 GPL
# Reinhold Kainhofer <reinhold@kainhofer.com> 2006 GPL
#
# simple script to replace QT3_SUPPORT find by indexOf
#
# usage:
#    convert-find-to-indexof.pl *.cpp
#
# CAUTION: This script replaces all calls to methods named 'find', even if the 
# object is not a class where the find method is deprecated. 
# I apply the script only to files that 
# throw a deprecated warning and compare the # of deprecated warnings to the nr 
# of replacements.
#
# NOTE: The results need a lot of manual tweaking!!!!


use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

foreach my $file (@ARGV) {
    chomp $file;
    next if functionUtilkde::excludeFile( $file);

    my $modified;
    my @necessaryIncludes = ();
    open(my $FILE, $file) or warn "We can't open file $file:$!\n";
    my @l = map {
      my $orig = $_;
      if ( m![\.>]find\s*\(! ) {
#       s/(.*\.)find/!XXX-\1-XXXYYY-\2-YYY/g;
#       s/(.*\.)find(\(.*\)\s*)==\s*([.*]\.)end\s*\(\s*\)/!\1contains\2/g;
        s/([^ ].*(.|->))find(\(.*\)\s*)==\s*\1end\s*\(\s*\)/!\1contains\3/g;
        s/([^ ]*(.|->))find(\(.*\)\s*)==\s*-1\s*/!\1contains\3/g;
        s/([^ ].*(.|->))find(\(.*\)\s*)!=\s*\1end\s*\(\s*\)/\1contains\3/g;
        s/([^ ]*(.|->))find(\(.*\)\s*)>=\s*0\s*/\1contains\3/g;
        s/([^ ]*(.|->))find(\(.*\)\s*)!=\s*-1\s*/\1contains\3/g;
# print "Old line:      \n$orig";
# print "New line:      \n$_====================\n";
      }
      
      if ( m![\.>]find\s*\(! && ! m/[mM]ap/ ) {
        s/([\.>])find(\s*)\(/\1indexOf\2(/g;
      }
      # third argument of find was changed from bool to  Qt::CaseSensitivity cs = Qt::CaseSensitive
      if ( my ($head, $parm1, $parm2, $ws, $parm3,$trailer) = m!^(.*ndexOf\s*)\(([^,]*),([^,]*),(\s*)([^, \)]*)(\s*\).*$)! ) {
        if ( $parm3 eq 'true' ) {
          $_ = "$head($parm1,$parm2,$ws"."Qt::CaseSensitive$trailer\n";
        } elsif ( $parm3 eq 'false' ) {
          $_ = "$head($parm1,$parm2,$ws"."Qt::CaseInsensitive$trailer\n";
        }
#if ( $orig ne $_ ) {
#  print "Old line:      \n$orig";
#  print "New line:      \n$_====================\n";
#}
      }
      $modified ||= $orig ne $_;
      $_;
    } <$FILE>;

    if ($modified) {
      print "Modified: $file\n";
      open (my $OUT, ">$file");
      print $OUT @l;
      close $OUT;
    }

}
functionUtilkde::diffFile( @ARGV );

