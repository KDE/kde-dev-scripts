#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2006 GPL
# Reinhold Kainhofer <reinhold@kainhofer.com> 2006 GPL
#
# Call with a file and a line number to change (by grepping compile logs)

# NOTE: This script assumes that the QGridLayout and Q[HV]BoxLayout constructors
#       are already converted to the new API, except for the possible
#       QLayout*parent first argument. Use the convert-qgridlayout.pl and
#       convert-qboxlayout.pl scripts for this.

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

my $file = $ARGV[0];
my $cline = $ARGV[1];

my $line = 0;

my $modified;
open(FILE, $file) or warn "We can't open file $file:$!\n";
while ( <FILE> )
{
   $line = $line + 1;
   if ($line == $cline) {
           my ($spaces, $trailer, $object, $call, $ws, $parent );
           if ( ($spaces, $trailer, $object, $call, $ws, $parent ) = m!^(\s*)(.*[\s\*]|)([a-zA-Z0-9]+)(\s*=\s*new Q[a-zA-Z0-9]*Layout[^(]*)\((\s*)(.*[^\s])\s*\);$! ) {
#print "Spaces: '$spaces', Trailer: '$trailer', Object: '$object', Call: '$call', WS: '$ws', Parent: '$parent'\n";
          $_  = "$spaces$trailer$object$call();\n";
          $_ .= "$spaces$parent->addItem($ws$object$ws);\n";
      }

   }
   print $_;
}

