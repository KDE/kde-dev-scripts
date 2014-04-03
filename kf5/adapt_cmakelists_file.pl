#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> 2014
# Modifies CMakeLists.txt in current directory to use kf5 macro

use strict;
my $file = "CMakeLists.txt";
open(my $FILE, "<", $file) || die;
my $modified = 0;
my @l = map {
  my $orig = $_;
  if (/kde4_add_ui_files/) {
     $_ =~ s/kde4_add_ui_files/qt5_wrap_ui/;
     $modified = 1;
  }
  if (/kde4_add_kcfg_files/) {
      $_ =~ s/kde4_add_kcfg_files/kconfig_add_kcfg_files/;
      $modified = 1;
  }
  
  $modified ||= $orig ne $_;
  $_;
} <$FILE>;

if ($modified) {
    open (my $OUT, ">", $file);
    print $OUT @l;
    close ($OUT);
}
