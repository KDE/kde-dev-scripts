#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2006 GPL
# Reinhold Kainhofer <reinhold@kainhofer.com> 2006 GPL
#
# simple script to replace the QT3_SUPPORT Q(H|V|)BoxLayout constructors:
#     QVBoxLayout ( QWidget * parent, int margin, int spacing = -1, const char * name = 0 ) 
#     QVBoxLayout ( QLayout * parentLayout, int spacing = -1, const char * name = 0 ) 
#     QVBoxLayout ( int spacing, const char * name = 0 )
# with the Qt4 API, which is
#     Q(H|V|)BoxLayout( QWidget* parent);
#     Q(H|V|)BoxLayout();
#     Q(H|V|)BoxLayout::set(Margin|Spacing|ObjectName)(...);
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
      my ($spaces, $trailer, $object, $call, $ws, $parent, $params);
      # $ws is the whitespace between brackets and contents
      # $spaces are the spaces at the beginning of the line
      # $trailer is the text between the spaces at the beginning and the object name
      # $object is the layout object (for the set* calls that we need to add)
      # $call is the actual constructor call (e.g. " = new QVBoxLayout ")
      # $parent is the first argument to the constructor, unless it starts with a number
      # $params is everything after the parent layout/widget until the end of the bracket
      #
      # this code assumes the constructor is a call on its own line, i.e. ends with new QVBoxLayout(...);
      $parent = "";
      if (  # Either parent widget/layout or first param is rows (=starts with number)
        ( ($spaces, $trailer, $object, $call, $ws, $parent, $params) = m!^(\s*)(.*[\s\*]|)([a-zA-Z0-9]+)(\s*=\s*new Q[HV]BoxLayout[^(]*)\((\s*)([^0-9 ][^,]*|0),\s*(.*[^\s])\s*\);$! ) ||
        ( ($spaces, $trailer, $object, $call, $ws,          $params) = m!^(\s*)(.*[\s\*]|)([a-zA-Z0-9]+)(\s*=\s*new Q[HV]BoxLayout[^(]*)\((\s*)([0-9][^,]*,\s*.*[^\s])\s*\);$! ) ) {
# print "Spaces: '$spaces', Trailer: '$trailer', Object: '$object', Call: '$call'";
# print "WS: '$ws', Parent: '$parent'\n";

# print "Params '$params'\n";
        if ( $parent eq "0" ) { $parent = ""; }
#         if ( length($parent)>0 && $parent != 0) {
# print "Parent: '$parent', \$parent:";
# if ( length($parent) ) {
#   print "In IF";
# } else {
#   print "NOT in IF";
# }
        if ( $parent ) {
          $_ = "$spaces$trailer$object$call($ws$parent$ws);\n";
        } else {
          $_ = "$spaces$trailer$object$call();\n";
        }
        my ( $margin, $space, $name );
        my @parms = split( /,\s*/, $params );
# print "Params: ";
# print join(" - ", @parms );
# print "\n";
        if ( scalar( @parms ) >= 3 ) {
          # All params are given: margin, spacing, name
          ( $margin, $space, $name ) = @parms;
        } else {
          if ( !defined($parent) ) { # constructor without parent layout/widget => spacing, name
            $space = $parms[0] if ( scalar( @parms ) >= 1 );
            $name = $parms[1] if ( scalar( @parms ) >= 2 );
          } elsif ( scalar( @parms ) >= 2 && @parms[1] =~ m/^"/ ) {
            # Second parameter is the name -> First must be the spacing
            $space = $parms[0];
            $name = $parms[1];
          } elsif ( scalar( @parms ) >= 2 ) { # fourth param is not name
            $margin = $parms[0];
            $space = $parms[1];
          } else {
            $space = $parms[0] if ( scalar( @parms ) >= 1 );
          }
        }
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

