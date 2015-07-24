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
           if (/KApplication a/) {
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
           if (/(\w+)\.addCredit\s*\(/) {
              my $var = $1;
              if ( defined $varname{$var} ) {
                  s/ki18n/i18n/g;
                  s/KLocalizedString\b/QString/g;
              }
           }
           if (/(\w+)\.setTranslator\s*\(/) {
              my $var = $1;
              if ( defined $varname{$var} ) {
                  s/ki18n/i18n/g;
                  s/KLocalizedString\b/QString/g;
              }
           }


           s/K4AboutData/KAboutData/g;
           s/KAboutData::License_/KAboutLicense::/;
        }
        if (/KCmdLineOptions (\w*)/) {
            $opt = $1;

            s/KCmdLineOptions /QCommandLineParser /;
            s/$opt/parser/;
            if (defined $QCommandLineParserAdded) {
              $_ = "";
            } else {
              if (defined $port_kapplicationAndK4AboutData) {
                 $_ .= "    QApplication app(argc, argv); // PORTING SCRIPT: move this to before the KAboutData initialization\n";
                 $_ .= "    KAboutData::setApplicationData(aboutData);\n";
              }
              $_ .= "    parser.addVersionOption();\n";
              $_ .= "    parser.addHelpOption();\n";
              if ( defined $use_aboutdata) {
                $_ .= "    //PORTING SCRIPT: adapt aboutdata variable if necessary\n";
                $_ .= "    aboutData.setupCommandLine(&parser);\n";
              }
              $_ .= "    parser.process(app); // PORTING SCRIPT: move this to after any parser.addOption\n";
              if ( defined $use_aboutdata) {
                $_ .= "    aboutData.processCommandLine(&parser);\n";
              }
            }
            $QCommandLineParserAdded = 1;

        } elsif (defined $opt && /KCmdLineArgs::addCmdLineOptions\s*\(\s*$opt\s*\)/ || /KCmdLineArgs::init/) {
           my $addNewAboutData;
           my $regexp = qr/
             ^(.*)                        # (1) Indentation
              KCmdLineArgs::init\s*\((.*)\)   # (2)  KCmdLineArgs::init(...,...,...,...);
              (.*)$                         # (3) afterreg
              /x; # /x Enables extended whitespace mode
           if (my ($indent, $argument, $afterreg) = $_ =~ $regexp) {
              my ($argc, $argv, $appname, $catalog, $programname, $version, $description);
              my $constructor_regexp = qr/
                                 ^([^,]*)\s*        # argc
                                 ,\s*([^,]*)\s*     # argv
			         ,\s*([^,]*)\s*     # appname
                                 ,\s*([^,]*)         # catalog
                                 ,\s*([^,]*)         # programname
                                 ,\s*([^,]*)         # version
                                 (?:,\s*([^,]*))?    # description
                                 (.*)$              # after
                                 /x;
              if ( ($argc, $argv, $appname, $catalog, $programname, $version, $description) = $argument =~ $constructor_regexp ) {
                 $appname =~ s/ki18n/i18n/;
                 $appname =~ s/ki18n/i18n/;
                 $programname =~ s/ki18n/i18n/;
                 if ( $appname =~ /^\"/ ) {
                    if ( $appname =~ /QLatin1String/ ) {
                       #nothing
                    } else {
                       $appname = "QLatin1String($appname)";
                    }
                 }
                 if ( $version =~ /QLatin1String/ ) {
                    #nothing
                 } else {
                    $version = "QLatin1String($version)";
                 }
                 $_ = $indent . "KAboutData aboutData( $appname, $programname, $version);\n";
                 if (defined $description) {
                    warn "DESCRIPION : $description\n";
                    $description =~ s/^,//;
                    $description =~ s/ki18n/i18n/;
                    $_ .= $indent . "aboutData.setShortDescription($description);\n";
                 }
                 $addNewAboutData = 1;
              } else {
                 $_ = "";
              }
            }

            if ( defined $QCommandLineParserAdded) {
               if (defined $addNewAboutData) {
                  #nothing
               } else {
                  $_ = "";
               }
            } else {
              if (defined $port_kapplicationAndK4AboutData) {
                 $_ .= "    QApplication app(argc, argv); // PORTING SCRIPT: move this to before the KAboutData initialization\\n";
                 $_ .= "    QCommandLineParser parser;\n";
                 $_ .= "    KAboutData::setApplicationData(aboutData);\n";
              }
              $_ .= "    parser.addVersionOption();\n";
              $_ .= "    parser.addHelpOption();\n";
              if ( defined $use_aboutdata) {
                $_ .= "    //PORTING SCRIPT: adapt aboutdata variable if necessary\n";
                $_ .= "    aboutData.setupCommandLine(&parser);\n";
              }
              $_ .= "    parser.process(app); // PORTING SCRIPT: move this to after any parser.addOption\n";
              if ( defined $use_aboutdata) {
                $_ .= "    aboutData.processCommandLine(&parser);\n";
              }
            }
            $QCommandLineParserAdded = 1;
        } elsif (defined $opt && s/(.*)$opt\.add\s*\(\s*"([^\"]*)"\s*\)/$1$opt/) { # short option

            $short = "QLatin1String(\"$2\") << ";
            $_ = "" if (!/\.add/);
            warn "$file: Be sure that option is added \'$2\'\n";
        }
        if (defined $opt && /(.*)$opt.add\s*\(\s*"([^\"]*)"\s*,\s*k(i18nc?)\((.*)\)\s*(?:,\s*([^\)]*))?\)/) {
            my $prefix = $1; # e.g. indent
            my $name = $2;
            my $i18n = $3;
            my $description = $4;
            my $defaultValue = $5;
            my $trail = "";
            if ($name =~ /(\w*) <(.*)>/) { # "stylesheet <xsl>"
                $name = $1;
                $trail = ", QLatin1String(\"$2\")";
            }
            if (defined $defaultValue) {
                if ( $defaultValue =~ /QLatin1String/ ) {
                   $trail .= ", $defaultValue";
                } else {
                    $trail .= ", QLatin1String($defaultValue)";
                }
            } elsif ($name =~ /^no/) { # negative option, e.g. --nosignal
                $negatedOptions{$name} = 1;
            }
            my $translate = defined $use_tr ? "QCoreApplication::translate($context, $description)" : "$i18n($description)";
            if ($name =~ s/^\+//) {
                $_ = "${prefix}parser.addPositionalArgument(QLatin1String(\"$name\"), $translate$trail);\n";
            } else {
                $_ = "${prefix}parser.addOption(QCommandLineOption(QStringList() << ${short}QLatin1String(\"$name\"), $translate$trail));\n";
            }
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
      functionUtilkde::addIncludeInFile($file, "KLocalizedString");

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
