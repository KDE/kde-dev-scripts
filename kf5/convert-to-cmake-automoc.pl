#!/usr/bin/perl

# Kevin Funk <kfunk@kde.org> (2015)
#
# Attempts to remove lines such as '#include "<basename>.moc"' from cpp files
#
# Automoc conventions:
# - include moc_<basename>.cpp <=> Q_OBJECT/Q_GADGET inside header
# - include <basename>.moc     <=> Q_OBJECT/Q_GADGET inside source file
#
# Additionally, if K_PLUGIN_FACTORY is used, we'll *have* to use '<basename>.moc'
#
# Now, if <basename>.moc is included, and the source file does *not* need a moc run,
# CMake's automoc still performs a moc run and moc will give warnings, such as:
# "foo.cpp:0: Note: No relevant classes found. No output generated."
# => Remove the include in this case
#
# More info: http://www.cmake.org/cmake/help/v3.0/manual/cmake-qt.7.html#automoc
#
# Usual invocation:
#   find -iname "*.cpp" | xargs kde-dev-scripts/kf5/convert-to-cmake-automoc.pl

use strict;
use warnings;

use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {
    open(my $FILE, "<", $file) or die "We can't open file $file:$!\n";
    my $content = do { local $/; <$FILE> };

    my $regexRequiresMoc = qr/(Q_OBJECT|Q_GADGET|K_PLUGIN_FACTORY|EXPORT_KONTACT_PLUGIN|_WITH_JSON|K_EXPORT_PLASMA_RUNNER)/;

    my ($filenameWithoutExtension, $dirs,) = fileparse($file, qr/\.[^.]*/);

    # In some cases, we cannot just get rid off the '#include <basename>.moc',
    # and need to include '#include moc_<basename>.cpp'
    # E.g. in case when the moc-generated file needs information about a class declared inside the .cpp file
    # Try to detect these cases and include '#include moc_<basename>.cpp' instead
    my $headerFile = $dirs . "$filenameWithoutExtension.h";
    my $requiresHeaderMocInclude = (-e $headerFile) ?
        `grep Q_PRIVATE_SLOT -q $headerFile` :
        0;

    my $sourceMocFilename = "$filenameWithoutExtension.moc";
    my $requiresSourceMocInclude = ($content =~ /$regexRequiresMoc/);
    my $includesSourceMocInclude = ($content =~ /#include \"$sourceMocFilename\"/);

    # rewrite file and fix moc includes
    if (!$requiresSourceMocInclude && $includesSourceMocInclude) {
        open (my $OUT, ">", $file);
        # TODO: Refactor, then use functionUtilkde::removeIncludeInFile?
        # Cannot use, because it a) only accepts <>-style includes and b) creates redundant newlines
        for (split /^/, $content) {
            # strip or replace unwanted includes
            if (/#include \"$sourceMocFilename\"/) {
                if ($requiresHeaderMocInclude) {
                    print $OUT "#include \"moc_$filenameWithoutExtension.cpp\"\n";
                }
                next; # remove line
            }

            print $OUT $_;
        }
        close ($OUT);
    }
}

functionUtilkde::diffFile( "@ARGV" );
