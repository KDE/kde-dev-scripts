#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2006 GPL
# Reinhold Kainhofer <reinhold@kainhofer.com> 2006 GPL
#
# simple script to replace QIODevice::at by either QIODevice::pos or QIODevice::seek
#
# usage:
#    convert-qiodevice_at.pl *.cpp
#
# CAUTION: This script replaces all calls to methods named 'at', even if the 
# object is not a QIODevice. So check whether the file contains only QIODevice 
# calls or also QList etc. Alternatively, I apply the script only to files that 
# throw a deprecated warning and compare the # of deprecated warnings to the nr 
# of replacements.


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
      if ( m![\.>]at\s*\(! ) {
        s/([\.>])at(\s*)\((\s*)\)/\1pos\2(\3)/g;
        s/([\.>])at(\s*)\((\s*[^)])/\1seek\2(\3/g;
# print "Old line:      \n$orig";
# print "New line:      \n$_====================\n";
      }
      s!.eof\s*\(\s*\)!.atEnd()!;
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

