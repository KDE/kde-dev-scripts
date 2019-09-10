#!/usr/bin/perl -w

# David Faure <faure@kde.org>
# Qt4 -> Qt5

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $infoVar;
    my $urlVar;

    # I don't use functionUtilkde::substInFile because it touches all files, even those which were not modified.
    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        my $regexpqFindChild = qr/
           ^(\s*)              # (1) Indentation
           (.*?)\s*=\s*           # (2) before
           qFindChild<([^>]*)>    # (3) class name
           \(\s*([&\w]+)\s*,\s*   # (4) variable
           (.*)$               # (5) end
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $before, $classname, $variable, $end) = $_ =~ $regexpqFindChild) {
           warn "found qFindChild \n";
           if ($variable =~ /^&/ ) {
             $variable =~ s/^&//;
             $_ = $indent . $before . " = " . $variable . ".findChild<$classname>(" . $end . "\n";
           } else {
             $_ = $indent . $before . " = " . $variable . "->findChild<$classname>(" . $end . "\n";
           }
        }

        my $regexpqFindChildren = qr/
           ^(\s*)              # (1) Indentation
           (.*?)                  # (2) before
           qFindChildren<([^>]*)> # (3) class name
           \(\s*([&\w]+)\s*\)    # (4) variable
           (.*)$               # (5) end
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $before, $classname, $variable, $end) = $_ =~ $regexpqFindChildren) {
           warn "found qFindChildren \n";
           if ($variable =~ /^&/ ) {
             $variable =~ s/^&//;
             $_ = $indent . $before . $variable . ".findChildren<$classname>()" . $end . "\n";
           } else {
             $_ = $indent . $before . $variable . "->findChildren<$classname>()" . $end . "\n";
           }
        }

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
