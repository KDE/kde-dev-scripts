#!/usr/bin/perl -w

# David Faure <faure@kde.org>
# KCommandLineArgs -> QCommandLineParser

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {
    my $context = "\"main\"";
    my $opt;

    functionUtilkde::substInFile {
        if (/KCmdLineOptions (\w*)/) {
            $opt = $1;
            s/KCmdLineOptions/QCommandLineParser/;
            s/$opt/parser/;
        } elsif (defined $opt && /(.*)$opt.add\s*\("([^\"]*)", ki18n\((.*)\)\)/) {
            my $prefix = $1; # e.g. indent
            my $str = $2;
            my $description = $3;
            my $trail = "";
            if ($str =~ /(\w*) <(.*)>/) { # "stylesheet <xsl>"
                $str = $1;
                $trail = ", QCommandLineOption::OneValue";
            }
            $_ = "${prefix}parser.addOption(QCommandLineOption(QStringList() << \"$str\", QCoreApplication::translate($context, $description)$trail));\n";
        }
        $_;
    } $file;
    if (`grep QCommand $file | grep -v '#include'`) {
      functionUtilkde::removeIncludeInFile($file, "kcmdlineargs.h");
      functionUtilkde::addIncludeInFile($file, "qcommandlineparser.h");
      functionUtilkde::addIncludeInFile($file, "qcommandlineoption.h");
    }
}

functionUtilkde::diffFile( "@ARGV" );
