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
    my $short = "";
    my $args;
    my %negatedOptions = ();

    functionUtilkde::substInFile {
        if (/KCmdLineOptions (\w*)/) {
            $opt = $1;
            s/KCmdLineOptions /QCommandLineParser \*/;
            s/$opt/parser = new QCommandLineParser/;
            $_ .= "    parser->addVersionOption(INSERT_VERSION_NUMBER_HERE);\n";
            $_ .= "    parser->addHelpOption(INSERT_DESCRIPTION_HERE);\n";
        } elsif (defined $opt && /KCmdLineArgs::addCmdLineOptions\s*\(\s*$opt\s*\)/ || /KCmdLineArgs::init/) {
            $_ = "";
        } elsif (defined $opt && /(.*)$opt.add\s*\("([^\"]*)"\)/) { # short option
            $short = "\"$1\", ";
        } elsif (defined $opt && /(.*)$opt.add\s*\("([^\"]*)", ki18n\((.*)\)(?:,\s*([^\)]*))?\)/) {
            my $prefix = $1; # e.g. indent
            my $name = $2;
            my $description = $3;
            my $defaultValue = $4;
            my $trail = "";
            if ($name =~ /(\w*) <(.*)>/) { # "stylesheet <xsl>"
                $name = $1;
                $trail = ", \"$2\"";
            }
            if (defined $defaultValue) {
                $trail .= ", false, $defaultValue";
            } elsif ($name =~ /^no/) { # negative option, e.g. --nosignal
                $negatedOptions{$name} = 1;
            }
            $_ = "${prefix}parser->addOption(QCommandLineOption(QStringList() << $short\"$name\", QCoreApplication::translate($context, $description)$trail));\n";
            $short = "";
        } elsif (/KCmdLineArgs\s*\*(\w*)\s*=\s*KCmdLineArgs::parsedArgs\(\)/) {
            $args = $1;
            $_ = "";
        } else {
            s/KCmdLineArgs::qtArgc\(\)/argc/;
            s/KCmdLineArgs::qtArgv\(\)/argv/;
            if (defined $args) {
                s/${args}\->getOptionList/parser->arguments/;
                s/${args}\->getOption/parser->argument/;
                s/${args}\->isSet/parser->isSet/;
                s/${args}\->count/parser->remainingArguments().count/;
                if (/arguments?\(\"(\w*)/ || /isSet\(\"(\w*)/) {
                    my $optionName = $1;
                    if (defined $negatedOptions{"no$optionName"}) {
                        s/$/\/\/ TODO: negate check/;
                        s/$optionName/no$optionName/g;
                    }
                }
            }
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
