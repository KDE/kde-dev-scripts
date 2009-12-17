#!/usr/bin/perl

# Montel Laurent <montel@kde.org> 
# This program allows to add custom widget to ui files
# for example : add-customwidget.pl OptionsDlgWidget.ui KIntSpinBox knuminput.h

use File::Basename;
use lib dirname( $0 );
use functionUtilkde; 
my $numberArgument = @ARGV;
"@ARGV" =~ /-h/ || ( $numberArgument ne 3 ) and die "usage: add-customwidget.pl <file> <name of custom widget> <include file for custom widget> \n";

my $namecustom=$ARGV[1];
my $nameheader=$ARGV[2];
functionUtilkde::substInFile {
	    s!</includes>!</includes>\n<customwidgets>\n\t<customwidget>\n\t\t<class>$namecustom</class>\n\t\t<header location="global">$nameheader</header>\n\t\t<container>0</container>\n\t</customwidget>\n</customwidgets>\n!;
} $ARGV[0];

functionUtilkde::diffFile( "$ARGV[0]" );
