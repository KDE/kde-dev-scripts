#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KFileDialog::getOpenFileName(...) => QFileDialog::getOpenFileName(...)
# find -iname "*.cpp" -o -iname "*.h" |xargs kde-dev-scripts/kf5/convert-kfiledialog.pl
# TODO need to improve it.
use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my %varname = ();
    my $needQFileDialog;
    my @l = map {
        my $orig = $_;

        #const QString fileName = KFileDialog::getOpenFileName( KUrl(), QString(), d->wParent, i18n("Attach File" ) );

        my $regexp = qr/
           ^(\s*)                        # (1) Indentation, possibly "Classname *" (the ? means non-greedy)
           (.*?)                         # (2) Possibly "Classname *" (the ? means non-greedy)
           (\w+)                         # (3) variable name
           \s*=\s*                       #   assignment
           KFileDialog::getOpenFileName\s*\((.*)\)  # (4)  new KPushButton(...,...,...,...);
           (.*)$                         # (5) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $var, $argument, $afterreg) = $_ =~ $regexp) {
           warn "QFileDialog::getOpenFileName found\n";
           my $constructor_regexp = qr/
                                 ^([^,]*)         # Url
                                 (?:,\s*([^,]*))?        # filter
                                 (?:,\s*([^,]*))?        # parent
                                 (?:,\s*([^,]*))?        # caption
                                 (.*)$              # after
                                 /x;
           my ($url, $filter, $parent, $caption, $after);
           if ( ($url, $filter, $parent, $caption, $after) = $argument =~  $constructor_regexp ) {
              $_ = $indent . $left . $var . " = QFileDialog::getOpenFileName(";
              if (defined $parent) {
                 $_ .= "$parent";
              } else {
                 $_ .= "0";
              }
              if (defined $caption) {
                 $_ .= ", $caption";
              } else {
                 $_ .= ", QString()";
              }
              if (defined $url) {
                 if ($url eq "KUrl()") {
                    $_ .= ", QString()";
                 } else {
                    $_ .= ", $url";
                 }
              } else {
                 $_ .= ", QString()";
              }
              if (defined $filter) {
                 $_ .= ", $filter);\n";
              } else {
                 $_ .= "QString());\n";
              }
              $needQFileDialog = 1;
           }

        }

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        if ($needQFileDialog) {
           functionUtilkde::addIncludeInFile($file, "QFileDialog");
        }
    }
}

functionUtilkde::diffFile( "@ARGV" );
