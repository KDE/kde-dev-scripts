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
  if (/kde4_add_executable/) {
      $_ =~ s/kde4_add_executable/add_executable/;
      $modified = 1;
  }
  if (/KDE4_ENABLE_EXCEPTIONS/) {
      $_ =~ s/set\s*\(\s*CMAKE_CXX_FLAGS\s*\"\$\{CMAKE_CXX_FLAGS\} \$\{KDE4_ENABLE_EXCEPTIONS\}\"\)/kde_enable_exceptions\(\)/;
      $modified = 1;
  } 
  if (/qt4_add_dbus_adaptor/) {
      $_ =~ s/qt4_add_dbus_adaptor/qt5_add_dbus_adaptor/;
      $modified = 1;
  }
  if (/qt4_wrap_ui/) {
      $_ =~ s/qt4_wrap_ui/qt5_wrap_ui/;
      $modified = 1;
  }
  #kde4_add_plugin(kio_mbox ${kio_mbox_PART_SRCS})
  my $regexp = qr/
               ^(\s*)                  # (1) Indentation
               kde4_add_plugin\s*\(    # 
               \s*([^ ]*)\s*           # (2) libname
               (.*)$                   # (3) end
               /x; # /x Enables extended whitespace mode
  if (my ($indent, $libname, $end) = $_ =~ $regexp) {
     $_ = $indent . "add_library($libname MODULE " . $end;
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
