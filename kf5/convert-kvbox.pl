#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# K[V|H]Box -> QWidget + QH|VBoxLayout
# find -iname "*.cpp" -o -iname "*.h"|xargs kde-dev-scripts/kf5/convert-kvbox.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    # Key = variable name of the parent QWidget
    # Value = variable name of the associated layout
    my %varname = ();

    my $modified;
    my $needQHBoxLayout;
    my $needQVBoxLayout;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        my $regexp = qr/
           ^(\s*)            # (1) Indentation
           (.*?)             # (2) Possibly "Classname *" (the ? means non-greedy)
           (\w+)             # (3) variable name
           \s*=\s*           #     assignment
           new\s+            #     new
           (K[HV]Box)\s*    # (4) KHBox or KVBox
           /x; # /x Enables extended whitespace mode
         if (my ($indent, $left, $var, $classname) = $_ =~ $regexp) {
           s/K[HV]Box/QWidget/g;
           my $mylayoutname = $classname eq "KHBox" ? "${var}HBoxLayout" : "${var}VBoxLayout";
           $mylayoutname =~ s/^m?_//; # coding style: this is a local var
           my $mylayoutclass = $classname eq "KHBox" ? "QHBoxLayout" : "QVBoxLayout";
           if ( defined $varname{$var}) {
              $_ .= $indent . "${mylayoutname} = new $mylayoutclass($var);" . "\n";
           } else {
              $_ .= $indent . $mylayoutclass . " *${mylayoutname} = new $mylayoutclass($var);" . "\n";
           } 
           $_ .= $indent . "${mylayoutname}->setMargin(0);\n";
           $varname{$var} = ${mylayoutname};
           if ($mylayoutclass eq "QHBoxLayout") {
              $needQHBoxLayout = 1;
           } else {
              $needQVBoxLayout = 1;
           }
        }
        my $widget_regexp = qr/
           ^(\s*)            # (1) Indentation
           (.*?)             # (2) Possibly "Classname *" (the ? means non-greedy)
           (\w+)             # (3) variable name
           \s*=\s*           #     assignment
           new\s+            #     new
           (\w+)\s*          # (4) classname
           ${functionUtilkde::paren_begin}5${functionUtilkde::paren_end}  # (5) (args)
           /x; # /x Enables extended whitespace mode
       if (my ($indent, $left, $var, $classname, $args) = $_ =~ $widget_regexp) {
           # Extract last argument
           #print STDERR "left=$left var=$var classname=$classname args=$args\n";
           my $extract_parent_regexp = qr/
            ^\(
            (?:.*?)                # args before the parent (not captured, not greedy)
            \s*(\w+)\s*            # (1) parent 
            (?:,\s*\"[^\"]*\"\s*)? # optional: object name
             \)$
            /x; # /x Enables extended whitespace mode      
           if (my ($lastArg) = $args =~ $extract_parent_regexp) {
              print STDERR "extracted parent=" . $lastArg . "\n";
              my $mylayoutname = $varname{$lastArg};
              if (defined $mylayoutname) {
                  $_ .= $indent . $mylayoutname . "->addWidget(" . $var . ");" . "\n";
               }
           } else {
              #warn $functionUtilkde::current_file . ":" . $functionUtilkde::current_line . ": couldn't extract last argument from " . $args . "\n";
          }
        }
 
        if (/(\w+)->setSpacing\s*\(.*/) {
          my $var = $1;
          my $mylayoutname = $varname{$var};
          if ( defined $mylayoutname ) {
              s/$var->setSpacing/$mylayoutname->setSpacing/;
          }
        }
        if (/(\w+)->setMargin\s*\(.*/) {
          my $var = $1;
          my $mylayoutname = $varname{$var};
          if ( defined $mylayoutname ) {
               s/$var->setMargin/$mylayoutname->setMargin/;
          }
        }
        if (/(\w+)->setStretchFactor\s*\(.*/) {
          my $var = $1;
          my $mylayoutname = $varname{$var};
          if ( defined $mylayoutname ) {
               s/$var->setStretchFactor/$mylayoutname->setStretchFactor/;
          }
        }



        s/#include <khbox\.h>/#include <QHBoxLayout>/;
        s/#include <kvbox\.h>/#include <QVBoxLayout>/;
        s/#include <KHBox>/#include <QHBoxLayout>/;
        s/#include <KVBox>/#include <QVBoxLayout>/;
        s/class KVBox;//;
        s/class KHBox;//;
        s/KVBox\s*\*/QWidget */;
        s/KHBox\s*\*/QWidget */;




        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        if ( $needQVBoxLayout) {
           functionUtilkde::addIncludeInFile($file, "QVBoxLayout");
        }
        if ( $needQHBoxLayout) {
           functionUtilkde::addIncludeInFile($file, "QHBoxLayout");
        }
    }
}

functionUtilkde::diffFile( "@ARGV" );
