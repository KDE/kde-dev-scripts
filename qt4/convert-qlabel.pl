#!/usr/bin/perl

# Laurent Montel <montel@kde.org> 2006 GPL
# Reinhold Kainhofer <reinhold@kainhofer.com> 2006 GPL
#
# Port the QT3_SUPPORT QLabel constructors to the Qt3 constructors.
#

use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

open(my $F, q(find [ac-z]* -name "*" | grep -v '\.svn/' |));
my $file;
while ($file = <$F>) {
    chomp $file;
    next if functionUtilkde::excludeFile( $file);

    my $modified;
    my @necessaryIncludes = ();
    open(my $FILE, $file) or warn "We can't open file $file:$!\n";
    my @l = map {
      my $orig = $_;
      my ($spaces, $trailer, $object, $call, $ws, $params );
      if ( ($spaces, $trailer, $object, $call, $ws, $params ) = m!^(\s*)(.*[\s\*]|)([a-zA-Z0-9_]+)(\s*=\s*new QLabel\s*)\((\s*)(.*[^\s])\s*\);$! ) {
print "Spaces: '$spaces', Trailer: '$trailer', Object: '$object', Call: '$call', WS: '$ws', Params: '$params'\n";
        my @parms = split( /,\s*/, $params );
        my $pws = $ws;

        my $buddy, my $text, my $parent, my $name, my $flags;

print "Case ";
        if ( $parms[0] =~ m/i18n\(.*/ ) {
print "1";
          $text=$parms[0];
          $parent=$parms[1];
          $name=$parms[2] if scalar(@parms)>2;
          $flags=$parms[3] if scalar(@parms)>3;
	  $pws=' ';
        } elsif ( scalar(@parms) == 5 ) {
          ($buddy, $text, $parent, $name, $flags) = @parms;
print "2";

        } elsif (scalar(@parms) == 4 ) {
          if ( $parms[3]=~m/Qt::/ ){
            ($text, $parent, $name, $flags) = @parms;
           } else {
             ($buddy, $text, $parent, $name) = @parms;
           }
print "3";

        } elsif (scalar(@parms) == 3) {
          if ( $parms[2]=~m/Qt::/ ){
            ($parent, $name, $flags) = @parms;
          } elsif ( $parms[2]=~m/^".*"$/ ) {
            ($text, $parent, $name) = @parms;
          } else {
            ($buddy, $text, $parent) = @parms;
          }
print "4";
        } elsif (scalar(@parms) == 2) {
          if ( $parms[1] =~ m/this/ ) {
            ($text, $parent) = @parms;
          } elsif ( $parms[1] =~ m/^".*"$/ ) {
            ($parent, $name) = @parms;
          } else {
            ($buddy, $text) = @parms;
          }
print "5";
        } elsif ( scalar(@parms) == 1 ) {
          $parent = $parms[0];
print "6";
        }
print "\n";

        $_ = "$spaces$trailer$object$call(";
        my $p="";
        $p.= "$text" if ( defined($text) );
        if ( defined($parent) ) {
          $p.= ",$pws" if ( defined($text) );
          $p.= "$parent";
          if ( defined($flags) ) { 
            $p.=",$ws$flags";
          }
        }
        if (defined($p)) { $_.="$ws$p$ws"; }
        $_.=");\n";
        if ( defined($name) ) {
          $_.="$spaces$object->setObjectName($ws$name$ws);\n";
        }
        if ( defined($buddy) ) {
          $_.="$spaces$object->setBuddy($ws$buddy$ws);\n";
        }
print "ORIG: $orig";
print "NEW:  $_";
# print "Original call: $call($ws$params$ws)\n";
print "Buddy: $buddy, Text: $text, Parent: $parent, Name: $name, Flags: $flags\n";


# QT4 API:
# QLabel ( QWidget * parent = 0, Qt::WFlags f = 0 )
# QLabel ( const QString & text, QWidget * parent = 0, Qt::WFlags f = 0 )
#
#
# DEPRECATED:
# QLabel ( QWidget * parent, const char * name, Qt::WFlags f = 0 )
# QLabel ( const QString & text, QWidget * parent, const char * name, Qt::WFlags f = 0 )
# QLabel ( QWidget * buddy, const QString & text, QWidget * parent = 0, const char * name = 0, Qt::WFlags f = 0 )

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

