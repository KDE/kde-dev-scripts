#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2006 GPL
# Reinhold Kainhofer <reinhold@kainhofer.com> 2006 GPL
#
# simple script to replace the QT3_SUPPORT QGridLayout constructors:
#     QGridLayout ( QWidget * parent, int nRows, int nCols = 1, int margin = 0, int space = -1, const char * name = 0 ) 
#     QGridLayout ( int nRows, int nCols = 1, int spacing = -1, const char * name = 0 ) 
#     QGridLayout ( QLayout * parentLayout, int nRows = 1, int nCols = 1, int spacing = -1, const char * name = 0 ) 
# with the Qt4 API, which is
#     QGridLayout( QWidget* parent);
#     QGridLayout::set(Margin|Spacing|ObjectName)(...);
# The row and col count is no longer needed at all.
#
# This script does NOT detect the QLayout*parentLayout case, which needs to be converted manually

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
      my ($spaces, $trailer, $object, $call, $ws,$parent, $params);
      # $ws is the whitespace between brackets and contents
      # $spaces are the spaces at the beginning of the line
      # $trailer is the text between the spaces at the beginning and the object name
      # $object is the grid layout object (for the set* calls that we need to add)
      # $call is the actual constructor call (e.g. " = new QGridLayout ")
      # $parent is the first argument to the constructor, unless it starts with a number
      # $params is everything after the parent layout/widget until the end of the bracket
      #
      # this code assumes the constructor is a call on its own line, i.e. ends with new QGridLayout(...);
      $parent = "";
      if (  # Either parent widget/layout or first param is rows (=starts with number)
        ( ($spaces, $trailer, $object, $call, $ws, $parent, $params) = m!^(\s*)(.*[\s\*]|)([a-zA-Z0-9]+)(\s*=\s*new QGridLayout[^(]*)\((\s*)([^0-9 ][^,]*|0),\s*(.*[^\s])\s*\);$! ) ||
        ( ($spaces, $trailer, $object, $call, $ws,          $params) = m!^(\s*)(.*[\s\*]|)([a-zA-Z0-9]+)(\s*=\s*new QGridLayout[^(]*)\((\s*)([0-9][^,]*,\s*.*[^\s])\s*\);$! ) ) {
# print "Spaces: '$spaces', Trailer: '$trailer', Object: '$object', Call: '$call'";
# print "WS: '$ws', Parent: '$parent'\n";

# print "Params '$params'\n";
        if ( $parent eq "0" ) { $parent = ""; }
        if ( $parent ) {
          $_ = "$spaces$trailer$object$call($ws$parent$ws);\n";
        } else {
          $_ = "$spaces$trailer$object$call();\n";
        }
        my ($rows, $cols, $margin, $space, $name );
        my @parms = split( /,\s*/, $params );

        if ( scalar( @parms ) >= 5 ) {
          # All params are given: rows, cols, margin, spacing, name
          ($rows, $cols, $margin, $space, $name) = @parms;
        } else {
          # rows and cols are always the first two.
          $rows = $parms[0] if ( scalar( @parms ) >= 1 );
          $cols = $parms[1] if ( scalar( @parms ) >= 2 );
          if ( !defined($parent) ) { # constructor without parent layout/widget => rows, cols, spacing, name
            $space = $parms[2] if ( scalar( @parms ) >= 3 );
            $name = $parms[3] if ( scalar( @parms ) >= 4 );
          } elsif ( scalar( @parms ) >= 4 && @parms[3] =~ m/^"/ ) {
            # fourth parameter is the name -> third must be the spacing
            $space = $parms[2];
            $name = $parms[3];
          } elsif ( scalar( @parms ) >= 4 ) { # fourth param is not name
            $margin = $parms[2];
            $space = $parms[3];
          } else {
            $space = $parms[2] if ( scalar( @parms ) >= 3 );
          }
        }
# print "Row: $rows\nCols: $cols\n";
# print "Margin: $margin\nSpacing: $space\nName: $name\n";

        # We don't need the row/col count any more in the new Qt4 API!!!
        $_ .= "$spaces$object->setObjectName($ws$name$ws);\n" if ( defined($name) );
        $_ .= "$spaces$object->setSpacing($ws$space$ws);\n" if ( defined($space) );
        $_ .= "$spaces$object->setMargin($ws$margin$ws);\n" if ( defined($margin) );
# print "Old line:      \n$orig";
# print "New line:      \n$_====================\n";
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

