#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# adapt KJob:: class
# find -iname "*.cpp" -o -iname "*.h"|xargs kde-dev-scripts/kf5/convert-kjob.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;
#-    job->ui()->setWindow(mParent);
#+    KJobWidgets::setWindow(job, mParent);
        my $regexp = qr/
                      ^(\s*)                  # (1) Indentation
                      (.*?)                   # (2) variable name
                      \->ui\(\)->setWindow\s*\( 
                      (.*[^\)])               # (3) arguments
                      (.*)$                   # (4) end
                      /x; # /x Enables extended whitespace mode
        if (my ($indent, $variable, $argument, $end) = $_ =~ $regexp) {
            $_ = $indent . "KJobWidgets::setWindow($2, $3" . $end;
        }

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        functionUtilkde::addIncludeInFile($file, "KJobWidgets/KJobWidgets");
    }
}

functionUtilkde::diffFile( "@ARGV" );
