#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2006 GPL
# Reinhold Kainhofer <reinhold@kainhofer.com> 2006 GPL
# 
# Heuristically try to detect Q*Layout constructors where the parent is a layout.
# Whenever the parent matches .*[lL]ayout.*, assume the parent is a QLayout* and
# port the constructor to the new API.
#
# NOTE: This script assumes that the QGridLayout and Q[HV]BoxLayout constructors
#       are already converted to the new API, except for the possible
#       QLayout*parent first argument. Use the convert-qgridlayout.pl and
#       convert-qboxlayout.pl scripts for this.

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

open(my $F, q(find -name "*" | grep -v '\.svn/' |));
my $file;
while ($file = <$F>) {
    chomp $file;
    next if functionUtilkde::excludeFile( $file);

    my $modified;
    my @necessaryIncludes = ();
    open(my $FILE, $file) or warn "We can't open file $file:$!\n";
    my @l = map {
      my $orig = $_;
      my ($spaces, $trailer, $object, $call, $ws, $parent );
      if ( ($spaces, $trailer, $object, $call, $ws, $parent ) = m!^(\s*)(.*[\s\*]|)([a-zA-Z0-9]+)(\s*=\s*new Q[a-zA-Z0-9]*Layout[^(]*)\((\s*)(.*[^\s])\s*\);$! ) {
print "Spaces: '$spaces', Trailer: '$trailer', Object: '$object', Call: '$call', WS: '$ws', Parent: '$parent'\n";
        if ( $parent =~ m![lL]ayout! && $parent !~ m![wW]idget! ) {
          $_  = "$spaces$trailer$object$call();\n";
          $_ .= "$spaces$parent->addItem($ws$object$ws);\n";
        }
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
functionUtilkde::diffFile( <$F> );

