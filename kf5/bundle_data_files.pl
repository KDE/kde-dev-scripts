#!/usr/bin/perl -w

# Changes from installing xmlgui5 files to bundling them into the resource.

use strict;
my $file = "CMakeLists.txt";
my $xmlgui_files;
my %qrc_files;
my %filesForApp = ();
my $modified = 0;
open(my $FILE1, "<", $file) || die;
while (<$FILE1>) {
    if (/qt5_add_resources\s*\(.*([\w]+\.qrc)/i) {
        $qrc_files{$1} = 1; # TODO the key should be the app name, and $1 should be the value
    }
}
close($FILE1);

open(my $FILE, "<", $file) || die;
my @l = map {
  my $orig = $_;

  if (/install\s*\(\s*FILES \s*((?:[\w\-\/]*\.rc\s*)*)\s*DESTINATION \s*\$\{KDE_INSTALL_KXMLGUI5DIR\}\/(\w+)/i) {
      $xmlgui_files = "$1";
      my $appname = $2;
      print STDERR "Found xmlgui files $xmlgui_files in $appname\n";
      $filesForApp{$appname} = $xmlgui_files;
      $_ = ""; # delete line

      # while here, insert line about new qrc
      my $qrc_file = "$appname.qrc";
      if (not defined $qrc_files{$qrc_file}) {
          $qrc_files{$qrc_file} = 1;
          $_ = "qt5_add_resources(${appname}_SRCS $qrc_file) #TODO move this to the right place, check the name of the variable\n";
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

if (defined $xmlgui_files) {
    foreach my $appname (keys(%filesForApp)) {
        my $qrc_file = "$appname.qrc";
        if (not -e $qrc_file) {
            open (my $QRC, ">", $qrc_file);
            print $QRC "<RCC>\n";
            print $QRC "<qresource prefix=\"/kxmlgui5/$appname\">\n";
            foreach my $xmlguifile (split(/ /, $filesForApp{$appname})) {
                print $QRC "<file>$xmlguifile</file>\n";
            }
            print $QRC "</qresource>\n";
            print $QRC "</RCC>\n";
            close($QRC);
            system("git add $qrc_file");
        } else {
            print STDERR "TODO: add these lines into $qrc_file, under prefix /xmlgui5/$appname\n";

            print STDERR "<qresource prefix=\"/kxmlgui5/$appname\">\n";
            foreach my $xmlguifile (split(/ /, $filesForApp{$appname})) {
                print STDERR "<file>$xmlguifile</file>\n";
            }
            print STDERR "</qresource>\n";
        }
    }
}

