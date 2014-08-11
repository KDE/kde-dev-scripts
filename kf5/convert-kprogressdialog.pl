#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KProgressDialog->QProgressDialog
# find -iname "*.cpp" -o -iname "*.h" |xargs kde-dev-scripts/kf5/convert-kprogressdialog.pl
# TODO need to improve it.
use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my %varname = ();
    my @l = map {
        my $orig = $_;
        my $regexp = qr/
           ^(\s*)                        # (1) Indentation, possibly "Classname *" (the ? means non-greedy)
           (.*?)                         # (2) Possibly "Classname *" (the ? means non-greedy)
           (\w+)                         # (3) variable name
           \s*=\s*                       #   assignment
           new\s+KProgressDialog
           (.*)$                         # (4) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $var, $afterreg) = $_ =~ $regexp) {
            warn "Found KProgressDialog $var \n";
            $varname{$var} = 1;
        }
        if (/(\w+)\-\>setAllowCancel\s*\(\s*false\s*\);/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
               s/$variable\-\>setAllowCancel\s*\(\s*false\s*\);/$variable\-\>setCancelButton\(0\);/;
            }
        }
        s/\bKProgressDialog\b/QProgressDialog/g;
        s/\<KProgressDialog\b\>/\<QProgressDialog>/ if (/#include/);
        s/\<kprogressdialog.h\>/\<QProgressDialog>/ if (/#include/);
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
