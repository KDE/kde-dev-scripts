#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KTemporaryFile->QTemporaryFile
# find -iname "*.cpp" -o -iname "*.h" |xargs kde-dev-scripts/kf5/convert-ktemporaryfile.pl

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

        my $regexpKMenuLocal = qr/
          ^(\s*)           # (1) Indentation
          KTemporaryFile\s+
          (\w+)            # (2) variable name
          /x; # /x Enables extended whitespace mode
        if (my ($left, $var) = $_ =~ $regexpKMenuLocal) {
           $varname{$var} = 1;
           s/\bKTemporaryFile\b/QTemporaryFile/;
        }

        my $regexp = qr/
           ^(\s*)                        # (1) Indentation, possibly "Classname *" (the ? means non-greedy)
           (.*?)                         # (2) Possibly "Classname *" (the ? means non-greedy)
           (\w+)                         # (3) variable name
           \s*=\s*                       #   assignment
           new\s+KTemporaryFile\s*\((.*)\)        # (4)  new KTemporaryFile(...,...,...,...);
           (.*)$                         # (5) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $var, $argument, $afterreg) = $_ =~ $regexp) {
           #warn "KTemporaryFile found \'$var\' \'$argument\'\n";
           $varname{$var} = 1;
           s/\bKTemporaryFile\b/QTemporaryFile/;
        }

        my $regexpKMenuFunction = qr/
          (.*?)
          KTemporaryFile\s*\*
          (\w+)            # (2) variable name
          /x; # /x Enables extended whitespace mode
        if (my ($left, $var) = $_ =~ $regexpKMenuFunction) {
           #warn "Found KTemporaryFile in function!!!! $var\n";
           $varname{$var} = 1;
           s/\bKTemporaryFile\b/QTemporaryFile/;
        }

        if (/(\w+)\.setPrefix\s*\((.*)\)/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
               my $newContructor= $2 . "QLatin1Char('/') + QLatin1String(\"/myapp_XXXXXX.txt\")";
               warn "$file : KTemporaryFile uses setPrefix add '$newContructor'\n";
               
               $_ = "//code was $_";
               $_ .= "//Add to constructor and adapt if necessay: $newContructor \n";
               
           }
        }

        if (/(\w+)\->setPrefix\s*\((.*)\)/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
               my $newContructor= $2 . "QLatin1Char('/') + QLatin1String(\"/myapp_XXXXXX.txt\")";
               warn "$file : KTemporaryFile uses setPrefix add '$newContructor'\n";
               $_ = "//code was $_";
               $_ .= "//Add to constructor and adapt if necessay: $newContructor \n";
           }
        }
        if (/(\w+)\.setSuffix\s*\((.*)\)/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
               my $newContructor= "QDir::tempPath() + QLatin1Char('/') + QLatin1String($2)";
               warn "$file : KTemporaryFile uses setPrefix add '$newContructor'\n";
               $_ = "//code was $_";
               $_ .= "//Add to constructor and adapt if necessay: $newContructor \n";
           }
        }

        if (/(\w+)\->setSuffix\s*\((.*)\)/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
               my $newContructor= "QDir::tempPath() + QLatin1Char('/') + QLatin1String($2)";
               warn "$file :KTemporaryFile uses setPrefix add '$newContructor'\n";
               $_ = "//code was $_";
               $_ .= "//Add to constructor and adapt if necessay: $newContructor \n";
           }
        }


        s/\bKTemporaryFile\b/QTemporaryFile/g;
        s/\<KTemporaryFile\b\>/\<QTemporaryFile>/ if (/#include/);
        s/\<KTemporaryFile.h\>/\<QTemporaryFile>/ if (/#include/);
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
