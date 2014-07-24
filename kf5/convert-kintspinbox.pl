#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KIntSpinbox -> QSpinBox
# find -iname "*.cpp"|xargs kde-dev-scripts/kf5/convert-kintspinbox.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {
    my %varname = ();

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        my $regexp = qr/
           ^(\s*)                        # (1) Indentation, possibly "Classname *" (the ? means non-greedy)
           (.*?)                         # (2) Possibly "Classname *" (the ? means non-greedy)
           (\w+)                         # (3) variable name
           \s*=\s*                       #   assignment
           new\s+KIntSpinBox\s*\((.*)\)   # (4)  new KIntSpinBox(...,...,...,...);
           (.*)$                         # (5) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $var, $argument, $afterreg) = $_ =~ $regexp) {
          warn "KIntSpinBox found $var $argument\n";
          #KIntSpinBox(int lower, int upper, int singleStep, int value, QWidget *parent, int base = 10);

          $varname{$var} = 1;

          my ($lower, $upper, $singleStep, $value, $parent, $base, $after);
          my $constructor_regexp = qr/
                                 ^([^,]*)\s*        # lower
                                 ,\s*([^,]*)\s*     # upper
                                 ,\s([^,]*)         # singleStep
                                 ,\s([^,]*)         # value
                                 ,\s([^,]*)         # widget
                                 (?:,\s([^,]*))?    # base
                                 (.*)$              # after
                                 /x;
          if ( ($lower, $upper, $singleStep, $value, $parent, $base, $after) = $argument =~ $constructor_regexp ) {
             $_ = $indent . $left . $var . " = new QSpinBox($parent);" . $after . "\n";
           
             $_ .= $indent . $var . "->setMaximum($upper);" . $after . "\n";
             $_ .= $indent . $var . "->setMinimum($lower);" . $after . "\n";
             $_ .= $indent . $var . "->setSingleStep($singleStep);" . $after . "\n";
             $_ .= $indent . $var . "->setValue($value);" . $after . "\n";
             if ($base) {
                $_ .= $indent . $var . "->setDisplayIntegerBase($base);" . $after . "\n";
             }
          }
        }
       my $regexpRange = qr/
           ^(\s*)                        # (1) Indentation, possibly "Classname *" (the ? means non-greedy)
           (\w+)                         # (2) variable name
           \->setRange\s*\((.*)\)        # (3) argument
           (.*)$                         # (4) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $var, $argument, $afterreg) = $_ =~ $regexpRange) {
            warn "found setRange \'$var\'\n";
            if ( defined $varname{$var} ) {
                 my $argument_regexp = qr/
                                 ^([^,]*)\s*        # mini
                                 ,\s*([^,]*)\s*     # maxi
                                 ,\s([^,]*)         # singleStep
                                 (.*)$              # after
                                 /x;
                 if (my ($minimum, $maximum, $singleStep) = $argument =~ $argument_regexp) {
                    $_ = $indent . "$var->setRange($minimum, $maximum);\n";
                    $_ .=  $indent . "$var->setSingleStep($singleStep);\n";
                 }
            }
        }

        s/\bKIntSpinBox\b/QSpinBox/g;
        s/\<KIntSpinBox\b\>/\<QSpinBox>/ if (/#include/);
        s/\<kintspinbox.h\>/\<QSpinBox>/ if (/#include/);

        s!KIntSpinBox!QSpinBox!;
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
