#!/usr/bin/perl -w

# David Faure <faure@kde.org>
# KCommandLineArgs -> QCommandLineParser

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;
my $use_tr;
my $use_aboutdata;

# Set $use_tr to generate code that uses QCoreApplication::translate
# If it's not set, i18n will be used.
#$use_tr = 1;

# Set $use_aboutdata_tr to generate code that uses kaboutdata
$use_aboutdata = 1;

my $port_kapplicationAndK4AboutData;
# Set $port_kapplicationAndK4AboutData if you want to port k4aboutdata + kapplication
$port_kapplicationAndK4AboutData = 1;

foreach my $file (@ARGV) {
    my $context = "\"main\"";
    my $opt;
    my $short = "";
    my $args;
    my %negatedOptions = ();
    my $needRemoveKApplication;
    my %varname = ();
    my $QCommandLineParserAdded;
    my $needQCommandLineOption;
    functionUtilkde::substInFile {
        if (defined $port_kapplicationAndK4AboutData) {
           if (/KApplication app/) {
              $_ ="";
              $needRemoveKApplication = 1;
           }
           my $regexpK4AboutDataLocal = qr/
              ^(\s*)           # (1) Indentation
              K4AboutData\s+
              (\w+)            # (2) variable name
              /x; # /x Enables extended whitespace mode
           if (my ($left, $var) = $_ =~ $regexpK4AboutDataLocal) {
               $varname{$var} = 1;
               s/ki18n/i18n/g;
           }
           if (/(\w+)\.addAuthor\s*\(/) {
              my $var = $1;
              if ( defined $varname{$var} ) {
                  s/ki18n/i18n/g;
                  s/KLocalizedString\b/QString/g;
              }
           }
           s/K4AboutData::License_/KAboutLicense::/;
           s/K4AboutData/KAboutData/g;
        }
        if (/KCmdLineOptions (\w*)/) {
            $opt = $1;

            s/KCmdLineOptions /QCommandLineParser /;
            s/$opt/parser/;
            if (defined $QCommandLineParserAdded) {
              $_ = "";
            } else {
              if (defined $port_kapplicationAndK4AboutData) {
                 $_ .= "    QApplication app(argc, argv);\n";
                 $_ .= "    KAboutData::setApplicationData(aboutData);\n";
              }
              $_ .= "    parser.addVersionOption();\n";
              $_ .= "    parser.addHelpOption();\n";
              if ( defined $use_aboutdata) {
                $_ .= "    //PORTING SCRIPT: adapt aboutdata variable if necessary\n";
                $_ .= "    aboutData.setupCommandLine(&parser);\n";
              }
              $_ .= "    parser.process(app);\n";
              if ( defined $use_aboutdata) {
                $_ .= "    aboutData.processCommandLine(&parser);\n";
              }
            }
            $QCommandLineParserAdded = 1;

        } elsif (defined $opt && /KCmdLineArgs::addCmdLineOptions\s*\(\s*$opt\s*\)/ || /KCmdLineArgs::init/) {
            if ( defined $QCommandLineParserAdded) {
               $_ = "";
            } else {
              if (defined $port_kapplicationAndK4AboutData) {
                 $_ = "    QApplication app(argc, argv);\n";
                 $_ .= "    QCommandLineParser parser;\n";
                 $_ .= "    KAboutData::setApplicationData(aboutData);\n";
              }
              $_ .= "    parser.addVersionOption();\n";
              $_ .= "    parser.addHelpOption();\n";
              if ( defined $use_aboutdata) {
                $_ .= "    //PORTING SCRIPT: adapt aboutdata variable if necessary\n";
                $_ .= "    aboutData.setupCommandLine(&parser);\n";
              }
              $_ .= "    parser.process(app);\n";
              if ( defined $use_aboutdata) {
                $_ .= "    aboutData.processCommandLine(&parser);\n";
              }
            }
            $QCommandLineParserAdded = 1;
        } elsif (defined $opt && /(.*)$opt.add\s*\(\s*"([^\"]*)"\s*\)/) { # short option
            $_ = "";
            $short = "\"$2\" << ";
        } elsif (defined $opt && /(.*)$opt.add\s*\(\s*"([^\"]*)"\s*,\s*k(i18nc?)\((.*)\)\s*(?:,\s*([^\)]*))?\)/) {
            my $prefix = $1; # e.g. indent
            my $name = $2;
            my $i18n = $3;
            my $description = $4;
            my $defaultValue = $5;
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
            my $translate = defined $use_tr ? "QCoreApplication::translate($context, $description)" : "$i18n($description)";
            $_ = "${prefix}parser.addOption(QCommandLineOption(QStringList() << QLatin1String($short\"$name\"), $translate$trail));\n";
            $needQCommandLineOption = 1;
            $short = "";
        } elsif (/KCmdLineArgs\s*\*\s*(\w*)\s*=\s*KCmdLineArgs::parsedArgs\(\s*\)/) {
            $args = $1;
            $_ = "";
        } else {
            s/KCmdLineArgs::qtArgc\(\)/argc/;
            s/KCmdLineArgs::qtArgv\(\)/argv/;
            if (defined $args) {
                s/${args}\->getOptionList/parser.values/g;
                s/${args}\->getOption/parser.value/g;
                s/${args}\->isSet/parser.isSet/g;
                s/${args}\->count/parser.positionalArguments().count/;
                s/${args}\->usage\s*\(\)/parser.showHelp()/;
                s/${args}\->clear\s*\(\);//;
                s/KCmdLineArgs::usage\s*\(\)/parser.showHelp()/;
                if (/arguments?\(\"(\w*)/ || /isSet\(\"(\w*)/) {
                    my $optionName = $1;
                    if (defined $negatedOptions{"no$optionName"}) {
                        s/$/\/\/ TODO: negate check (and ensure nobody passes the no-op --$optionName argument)/;
                        s/$optionName/no$optionName/g;
                    }
                }
            }
        }
        $_;
    } $file;

    if (defined $needRemoveKApplication) {
      functionUtilkde::removeIncludeInFile($file, "KApplication");
      functionUtilkde::removeIncludeInFile($file, "kapplication.h");
      functionUtilkde::addIncludeInFile($file, "QApplication");
      functionUtilkde::removeIncludeInFile($file, "K4AboutData");
      functionUtilkde::removeIncludeInFile($file, "k4aboutdata.h");
      functionUtilkde::addIncludeInFile($file, "KAboutData");

    }

    if (`grep QCommand $file | grep -v '#include'`) {
      functionUtilkde::removeIncludeInFile($file, "kcmdlineargs.h");
      functionUtilkde::removeIncludeInFile($file, "KCmdLineArgs");
      functionUtilkde::removeIncludeInFile($file, "KCmdLineOptions");
      functionUtilkde::addIncludeInFile($file, "QCommandLineParser");
      if (defined $needQCommandLineOption) {
        functionUtilkde::addIncludeInFile($file, "QCommandLineOption");
      }
    }
}

functionUtilkde::diffFile( "@ARGV" );
