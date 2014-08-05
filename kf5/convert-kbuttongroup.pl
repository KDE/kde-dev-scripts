#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KButtonGroup -> QGroupBox + QButtonGroup
# find -iname "*.cpp" -o -iname "*.h"|xargs kde-dev-scripts/kf5/convert-kbuttongroup.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    # Key = variable name of the parent QWidget
    # Value = variable name of the associated layout
    my %varname = ();

    my $modified;
    my $buttonId=0;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
        my $regexp = qr/
           ^(\s*)            # (1) Indentation
           (.*?)             # (2) Possibly "Classname *" (the ? means non-greedy)
           (\w+)             # (3) variable name
           \s*=\s*           #     assignment
           new\s+KButtonGroup #     new
           /x; # /x Enables extended whitespace mode
         if (my ($indent, $left, $var) = $_ =~ $regexp) {
           s/KButtonGroup/QGroupBox/g;
           my $buttonGroupName = "$var" . "ButtonGroup";
           $_ .= $indent . "QButtonGroup *$buttonGroupName = new QButtonGroup($var);\n";
           $varname{$var} = ${buttonGroupName};
           warn "BECAREFULL, verify that each button is added to ButtonGroup => $buttonGroupName->addButton(..., id);\n";

        }
	if ( /\s*connect\s*\(\s*(\w+),\s*SIGNAL\s*\(\s*clicked\s*\(\s*int\s*\)/ ) {
           if (defined $varname{$1} ) {
              my $newValue = $varname{$1};
              s/$1/$newValue/;
              s/clicked/buttonClicked/;
           }
        }
        if ( /\s*connect\s*\(\s*(\w+),\s*SIGNAL\s*\(\s*released\s*\(\s*int\s*\)/ ) {
           if (defined $varname{$1} ) {
              my $newValue = $varname{$1};
              s/$1/$newValue/;
              s/released/buttonReleased/;
           }
        }
        if ( /\s*connect\s*\(\s*(\w+),\s*SIGNAL\s*\(\s*pressed\s*\(\s*int\s*\)/ ) {
           if (defined $varname{$1} ) {
              my $newValue = $varname{$1};
              s/$1/$newValue/;
              s/pressed/buttonPressed/;
           }
        }
        if ( /\s*connect\s*\(\s*(\w+),\s*SIGNAL\s*\(\s*changed\s*\(\s*int\s*\)/ ) {
           if (defined $varname{$1} ) {
              warn "$file: QButtonGroup doesn't have changed signal. Need to adapt it\n";
           }
        }

        if (/(\w+)\->selected\s*\(\)/) {
           if (defined $varname{$1} ) {
              my $newValue = $varname{$1};
              s/$1/$newValue/;
              s/selected/checkedId/;
           }
        }
        if (/(\w+)\->setSelected\b/) {
           if (defined $varname{$1} ) {
              my $newValue = $varname{$1};
              s/$1/$newValue/;
              s/setSelected/button/;
              s/;/->setChecked(true);/
           }
        }
        #TODO add buttons
        s/#include <kbuttongroup\.h>/#include <QGroupBox>/;
        s/#include <KButtonGroup>/#include <QGroupBox>/;
        s/class KButtonGroup;//;
        s/KButtonGroup\s*\*/QGroupBox */;
        s/KButtonGroup\s*\*/QGroupBox */;


        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
	functionUtilkde::addIncludeInFile($file, "QButtonGroup");

    }
}

functionUtilkde::diffFile( "@ARGV" );
