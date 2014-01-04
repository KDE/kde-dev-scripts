#!/usr/bin/perl -w

# Modifies CMakeLists.txt in the current directory, to install forwarding headers
# using ecm_generate_headers() instead of a manual list of lowercase headers.
#
# Remember to adjust things manually when using namespaced headers: 
#  - add PREFIX to the ecm_generate_headers call
#  - add that "/Prefix" to the directory in target_include_directories(... INTERFACE ...)
#  - add that lowercase prefix to the install dir for lowercase headers

use strict;
my %foundclasses = ();
my $name;
my $basename;
my $file = "CMakeLists.txt";
my $tid_done;
my $in_install_files;
my $devel_done;
my $install_dir_done;
my $modified = 0;
open(my $FILE, "<", $file) || die;

my @l = map {
  my $orig = $_;

  if (/generate_export_header\s*\(\s*(\w+)/) {
    $name = $1;
  }
  if (/BASE_NAME (\w+)/) {
    $basename = $1;
  }

  if (/target_link_libraries/ && !defined $tid_done) {
    $tid_done = 1;
    die "didn't find a generate_export_header line before target_link_libraries!" unless defined $name;
    $_ = "target_include_directories($name INTERFACE \"\$<INSTALL_INTERFACE:\${INCLUDE_INSTALL_DIR}/$basename>\")\n\n$_";
  }

  if (/install.\s*FILES\s*$/ || defined $in_install_files) {
    s/install\( FILES/install\(FILES/;
    $in_install_files = 1;
    if (/^\s*(\w+\/)?(\w+\.h)/ && !/export\.h/ && !/_p\.h/) {
      my $origline = $_;
      my $subdir = $1;
      my $header = $2;
      $subdir =~ s,/$,,;
      my $foundclass;
      my $multipleclasses = "";
      my $path = $subdir eq "" ? $header : "$subdir/$header";
      open(F, "$path") || print STDERR "ERROR: $path not found\n";
      while (<F>) {
        if (/^class .*(?:EXPORT|EXPORT_NOISE) \s*(\w+)/) {
          if (lc($1).".h" eq lc($header)) {
            $foundclass = $1;
          }
          $multipleclasses .= $1 . " ";
        }
        if (/^\s*namespace \s*(\w+)/ || /^typedef \w+ (\w+)/) {
          if (lc($1).".h" eq lc($header)) {
            $foundclass = $1;
            $multipleclasses .= $foundclass . " ";
          }
        }
      }
      close(F);
      if (defined $foundclass) {
        $foundclasses{$subdir} = "" unless defined $foundclasses{$subdir};
        $foundclasses{$subdir} .= "  " . $foundclass . "\n";
        #print STDERR "inserting into foundclasses{$subdir} : " . $foundclasses{$subdir} . "\n";
        $_ = "";
      } else {
        $multipleclasses =~ s/ $//; # remove trailing slash
        if ($multipleclasses =~ / /) {
          print STDERR "WARNING: multiple exported classes found in $path: $multipleclasses\n";
        } else {
          print STDERR "WARNING: No exported class or namespace found in $path, check for a template maybe?\n";
        }
        $_ = $origline;
      }
    }
  }

  if (/COMPONENT Devel/ && !defined $devel_done) {
    $devel_done = 1;
    undef $in_install_files;
    die unless defined $basename;
    $_ = "  \${${basename}_HEADERS}\n" . $_;
    s/\${INCLUDE_INSTALL_DIR} /\${INCLUDE_INSTALL_DIR}\/$basename /;
  }

  if (defined $devel_done && /\)/) {
    my $line = $_;
    foreach my $subdir (keys %foundclasses) {
      print STDERR "writing out classes for subdir '" . $subdir ."'\n";
      my $theclasses = $foundclasses{$subdir};
      $line .= "ecm_generate_headers(\n$theclasses\n";
      if ($subdir ne "") {
        $line .= "  RELATIVE $subdir\n";
      }
      $line .= "  MODULE_NAME $basename\n  REQUIRED_HEADERS ${basename}_HEADERS\n)\n";
    }
    $_ = $line;
    %foundclasses = ();
  }

  # happens earlier in the file, but must be last to avoid matching on "COMPONENT Devel"
  if (/install.TARGETS/ && !defined $install_dir_done) {
    $install_dir_done = 1;
    $_ = "<move ecm_generate_headers calls here>\ninstall(DIRECTORY \${CMAKE_CURRENT_BINARY_DIR}/$basename DESTINATION \${INCLUDE_INSTALL_DIR} COMPONENT Devel)\n\n" . $_;
  }

  $modified ||= $orig ne $_;
  $_;
} <$FILE>;

if ($modified) {
    open (my $OUT, ">", $file);
    print $OUT @l;
    close ($OUT);
}


