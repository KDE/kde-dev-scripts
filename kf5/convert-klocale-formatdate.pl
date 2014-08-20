#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KGlobal::locale().formatDate -> QLocale().toString
# find -iname "*.cpp"|xargs kde-dev-scripts/kf5/convert-klocale-formatdate.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
       
        if (/KLocale::global\(\)\-\>formatDate\s*\((.*)\)/) {   
           my $args = $1;
           warn "Found KGlobal::locale : $args\n";
           my $arg_regexp = qr/
                            ^([^,]*)\s*        # date
                            (?:,\s([^,]*))?    # option
                            (.*)$              # after
                            /x;
           if ( my ($date, $option, $after) = $args =~ $arg_regexp ) {
              if ( defined $option) {
                 warn "Option found $option\n";
              }
              warn "Argument ? : $date \n";
              s,KLocale::global\(\)\-\>formatDate\b,QLocale().toString,;
              s,KLocale::ShortDate,QLocale::ShortFormat,;
              s,KLocale::LongDate,QLocale::LongFormat,;
           } 
        }

        s,KLocale::global\(\)\-\>weekStartDay\(\),QLocale().firstDayOfWeek(),g;
        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        functionUtilkde::addIncludeInFile($file, "QLocale");
    }
}

functionUtilkde::diffFile( "@ARGV" );
