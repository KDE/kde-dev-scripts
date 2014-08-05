#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KMD5->QCryptographicHash(QCryptographicHash::Md5)
# find -iname "*.cpp" -o -iname "*.h" |xargs kde-dev-scripts/kf5/convert-kmd5.pl
# TODO need to improve it.
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

        my $regexp = qr/
          ^(\s*)           # (1) Indentation
          KMD5\s+
          (\w+)            # (2) variable name
          ${functionUtilkde::paren_begin}3${functionUtilkde::paren_end}  # (3) (args)         
          /x; # /x Enables extended whitespace mode
        if (my ($indent, $var, $argument) = $_ =~ $regexp ) {
           warn "VAR: $var , ARGUMENT $argument\n";
           $varname{$var} = 1;
           $_ = $indent . "QCryptographicHash $var(QCryptographicHash::Md5);\n";
           $_ .= $indent . "$var.addData$argument;\n";
        }

        if (/(\w+)\.base64Digest\s*\(\s*\)/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
              s/$var\.base64Digest\b/$var.result().toBase64/;
           }
        }
        if (/(\w+)\.hexDigest\s*\(\s*\)/) {
           my $var = $1;
           if ( defined $varname{$var} ) {
              s/$var\.hexDigest\b/$var.result().toHex/;
           }
        }


        s/\<KMD5\b\>/\<QCryptographicHash>/ if (/#include/);
        s/\<kmd5.h\>/\<QCryptographicHash>/ if (/#include/);
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
