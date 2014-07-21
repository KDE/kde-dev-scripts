#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KColorDialog::getColor => QColorDialog::getColor
# find -iname "*.cpp" -o -iname "*.h" |xargs kde-dev-scripts/kf5/convert-kcolordialog.pl
# TODO need to improve it.
use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my %varname = ();
    my %finalized_called = ();
    my @l = map {
        my $orig = $_;
        
        my $regexp = qr/
           ^(\s*)            # (1) Indentation
           (.*?)             # (2) Possibly "Classname *" (the ? means non-greedy)
           KColorDialog::getColor\s*\((.*)\) # 3 (args)
           (.*)
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $args, $endline) = $_ =~ $regexp) {
          #warn "Found kcolordialog::getColor \'$left\', argument: \'$args\'\n";
          my $constructor_regexp = qr/
                                 ^([^,]*)\s*        # color
                                 (?:,\s([^,]*))?    # defaultColor
                                 (?:,\s([^,]*))?    # widget parent
                                 (.*)$              # after
                                 /x;
          if (my ($color, $defaultcolor, $parent, $after) = $args =~ $constructor_regexp ) {
              #warn "Argument found : color \'$color\', defaultColor \'$defaultcolor\'\n";
              #if (defined $parent) {
              #   warn "We specified a parent \'$parent\'\n";
              #}
              $color =~ s, ,,g;
              $endline =~ s, ,,g;
              if (defined $defaultcolor) {
                  $defaultcolor =~ s, ,,g;
              }
              if ( $left =~ /^if\s*\(/ ) {
                 if (defined $defaultcolor) {
                    $_ = $indent . "$color = QColorDialog::getColor($defaultcolor";
                 } else {
                    $_ = $indent . "$color = QColorDialog::getColor(";
                 }
                 if ( defined $parent) {
                    $_ .= ", $parent";
                    if ( $parent =~ /\)/) {
                       $_ .= ";\n";
                    } else {
                       $_ .= ");\n";
                    }
                 } else {
                    $_ .= ");\n";
                 }
                 $_ .= $indent . "if ( $color.isValid() )";
                 if ($endline =~ /{$/) {
                    $_ .= " {";
                 }
                 $_ .= "\n";
                 #warn "code is in a if\n";
              } else {
                 if (defined $defaultcolor) {
                    $_ = $indent . "$color = QColorDialog::getColor($defaultcolor";
                 } else {
                    $_ = $indent . "$color = QColorDialog::getColor(";
                 }
                 if ( defined $parent) {
                    $_ .= ", $parent";
                    if ( $parent =~ /\)/) {
                       $_ .= ";\n";
                    } else {
                       $_ .= ");\n";
                    }
                 } else {
                    $_ .= ");\n";
                 }
                 $_ .= $indent . "if ( $color.isValid() ) {\n";
                 $_ .= $indent . "//PORTING SCRIPT: move old code when color was valid\n";
                 $_ .= $indent . "}\n";
                 warn "Need to adapt code in $file\n";
              }
          } 
        }

        s/\bKColorDialog\b/QColorDialog/g;
        s/\<KColorDialog\b\>/\<QColorDialog>/ if (/#include/);
        s/\<kcolordialog.h\>/\<QColorDialog>/ if (/#include/);
        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
    }
}

functionUtilkde::diffFile( "@ARGV" );
