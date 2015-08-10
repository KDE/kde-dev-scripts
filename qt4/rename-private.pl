#!/usr/bin/perl -w

# Usage: rename-private.pl foo.h

use strict;
use File::Basename;
use lib dirname( $0 );
use functionUtilkde;

my $file = $ARGV[0];
$file =~ s/\.h$//;

open(my $HEADER, "$file.h") or warn "Unable to open file $file.h:$!\n";
my $modified;
my $classname;
my $fwddecl;
my @l = map {
    my $orig = $_;
    if (/^class [A-Z_]+_EXPORT (\w+)/) {
        $classname = $1;
    }
    if (defined $classname && /class Private;/) {
        $_ = "";
        $fwddecl = "class ${classname}Private;";
    } elsif (defined $classname && /\bPrivate\s*/) {
        s/Private/${classname}Private/;
    }

    $modified ||= $orig ne $_;
    $_;
} <$HEADER>;
close $HEADER;

if ($modified) {
    open(my $OUT, ">$file.h");
    print $OUT @l;
    close $OUT;
}

if (defined $fwddecl) {
    functionUtilkde::addAfterAllIncludes("$file.h", $fwddecl);
}

if (defined $classname) {

    open(my $IMPL, "$file.cpp") or warn "Unable to open file $file.cpp:$!\n";

    undef $modified;

    @l = map {
    my $orig = $_;

    s/::Private/Private/g;
    s/\bPrivate/${classname}Private/g;

    $modified ||= $orig ne $_;
    $_;
    } <$IMPL>;
    close $IMPL;

    if ($modified) {
        open(my $OUT, ">$file.cpp");
        print $OUT @l;
        close $OUT;
    }

}
