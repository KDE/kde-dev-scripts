#!/usr/bin/perl -w

###############################################################################
# Sanity checks CMakeLists.txt files.                                         #
# Copyright (C) 2006 by Allen Winter <winter@kde.org>                         #
#                                                                             #
# This program is free software; you can redistribute it and/or modify        #
# it under the terms of the GNU General Public License as published by        #
# the Free Software Foundation; either version 2 of the License, or           #
# (at your option) any later version.                                         #
#                                                                             #
# This program is distributed in the hope that it will be useful,             #
# but WITHOUT ANY WARRANTY; without even the implied warranty of              #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the                #
# GNU General Public License for more details.                                #
#                                                                             #
# You should have received a copy of the GNU General Public License           #
# along with this program; if not, write to the Free Software                 #
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. #
#                                                                             #
###############################################################################
#
# A program to check KDE CMakeLists.txt files for common errors.
#
# Program options:
#   --help:          display help message and exit
#   --version:       display version information and exit
#

use strict;
use Getopt::Long;
use Cwd 'abs_path';

my($Prog) = 'cmakelint.pl';
my($Version) = '1.2';

my($help) = '';
my($version) = '';

exit 1
if (!GetOptions('help' => \$help, 'version' => \$version));

&Help() if ($help);
if ($#ARGV < 0){ &Help(); exit 0; }
&Version() if ($version);

my($f,$tot_issues);
$tot_issues=0;
for $f (@ARGV) {
  $tot_issues += &processFile($f);
}
exit $tot_issues;

sub processFile() {
  my($in) = @_;
  print "Processing $in:\n";
  open(IN,"$in") || die "Couldn't open $in";

  my($apath) = abs_path($in);
  my($in_kdelibs)=0;
  $in_kdelibs=1 if ($apath =~ m+/kdelibs/+);
  my($in_kdepimlibs)=0;
  $in_kdepimlibs=1 if ($apath =~ m+/kdepimlibs/+);
  my($top_of_module)=0;
  $top_of_module=1 if ($apath =~ m+/koffice/[a-zA-Z_1-9]*/CMakeLists.txt+); 
  $top_of_module=1 if ($apath =~ m+/kde(libs|pimlibs|base|accessibility|addons|admin|artwork|bindings|edu|games|graphics|multimedia|network|pim|sdk|toys|utils|develop|webdev)/[a-zA-Z_1-9]*/CMakeLists.txt+);
  $top_of_module=0 if ($apath =~ m+/(cmake|pics)/+);

  my(@lines) = <IN>;
  my($line);
  my($linecnt)=0;
  my($issues)=0;
  #look for "bad" stuff
  foreach $line (@lines) {
    $linecnt++;
    chomp($line);
    $line =~ s/#.*$//; #remove comments
    next if (! $line);                   #skip empty lines
    next if ($line =~ m/^[[:space:]]$/); #skip blank lines

    $issues += &checkLine($line,$linecnt,
			  '[[:space:]]{\$',
			  'replace "{$" with "${", or garbage detected');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]lib[[:space:]]*\)',
			  'replace "lib" with "${LIB_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${LIB_INSTALL_DIR}/\$',
			  'replace "${LIB_INSTALL_DIR}/${...}" with "${LIB_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]include[[:space:]]*\)',
			  'replace "include" with "${INCLUDE_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${INCLUDE_INSTALL_DIR}/\$',
			  'replace "${INCLUDE_INSTALL_DIR}/${...}" with "${INCLUDE_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]bin[[:space:]]*\)',
			  'replace "bin" with "${BIN_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${BIN_INSTALL_DIR}/\$',
			  'replace "${BIN_INSTALL_DIR}/${...}" with "${BIN_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
 			  'DESTINATION[[:space:]].*share/apps[[:space:]]*\)',
 			  'replace "share/apps" with "${DATA_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
 			  'DESTINATION[[:space:]]\${DATA_INSTALL_DIR}/\$',
 			  'replace "${DATA_INSTALL_DIR}/${...}" with "${DATA_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]].*share/autostart[[:space:]]*\)',
			  'replace "share/autostart" with "${AUTOSTART_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${AUTOSTART_INSTALL_DIR}/\$',
			  'replace "${AUTOSTART_INSTALL_DIR}/${...}" with "${AUTOSTART_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]].*/share/icons[[:space:]]*\)',
			  'replace "share/icons" with "${ICON_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${ICON_INSTALL_DIR}/\$',
			  'replace "${ICON_INSTALL_DIR}/${...}" with "${ICON_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]].*share/locale[[:space:]]*\)',
			  'replace "share/locale" with "${LOCALE_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${LOCALE_INSTALL_DIR}/\$',
			  'replace "${LOCALE_INSTALL_DIR}/${...}" with "${LOCALE_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]].*share/services[[:space:]]*\)',
			  'replace "share/services" with "${SERVICES_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${SERVICES_INSTALL_DIR}/\$',
			  'replace "${SERVICES_INSTALL_DIR}/${...}" with "${SERVICES_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]].*share/sounds[[:space:]]*\)',
			  'replace "share/sounds" with "${SOUND_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${SOUND_INSTALL_DIR}/\$',
			  'replace "${SOUND_INSTALL_DIR}/${...}" with "${SOUND_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'install_files[[:space:]]*\(',
			  'replace "install_files" with "install(FILES...)');
    $issues += &checkLine($line,$linecnt,
			  'INSTALL_FILES[[:space:]]*\(',
			  'replace "install_files" with "install(FILES...)');
    $issues += &checkLine($line,$linecnt,
			  'FILES[[:space:]]DESTINATION',
			  'missing list of files between FILES and DESTINATION');
    $issues += &checkLine($line,$linecnt,
			  'TARGETS[[:space:]]DESTINATION',
			  'missing list of files between TARGETS and DESTINATION');

    $issues += &checkLine($line,$linecnt,
			  'macro_bool_to_01[[:space:]]*\(.*[[:space:]][[:digit:]][[:space:]]*\)',
			  'do not use a digit as a variable');
    $issues += &checkLine($line,$linecnt,
			  'MACRO_BOOL_TO_01[[:space:]]*\(.*[[:space:]][[:digit:]][[:space:]]*\)',
			  'do not use a digit as a variable');


    # kdelibs variables
    if (! $in_kdelibs) {
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kdeui[[:space:]]',
		   'replace "kdeui" with "${KDE4_KDEUI_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kio[[:space:]]',
		   'replace "kio" with "${KDE4_KIO_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kdefx[[:space:]]',
		   'replace "kdefx" with "${KDE4_KDEFX_LIBS}"');
      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kdeprint[[:space:]]',
            'replace "kdeprint" with "${KDE4_KDEPRINT_LIBS}"');
      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kdesu[[:space:]]',
            'replace "kdesu" with "${KDE4_KDESU_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]khtml[[:space:]]',
            'replace "khtml" with "${KDE4_KHTML_LIBS}"');
      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kparts[[:space:]]',
            'replace "kparts" with "${KDE4_KPARTS_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kde3support[[:space:]]',
            'replace "kde3support" with "${KDE4_KDE3SUPPORT_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kutils[[:space:]]',
            'replace "kutils" with "${KDE4_KUTILS_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kdnssd[[:space:]]',
            'replace "kdnssd" with "${KDE4_KDNSSD_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]knewstuff[[:space:]]',
            'replace "knewstuff" with "${KDE4_KNEWSTUFF_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]knotifyconfig[[:space:]]',
            'replace "knotifyconfig" with "${KDE4_KNOTIFYCONFIG_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]threadweaver[[:space:]]',
            'replace "threadweaver" with "${KDE4_THREADWEAVER_LIBRARIES}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kspell2[[:space:]]',
            'replace "kspell2" with "${KDE4_KSPELL2_LIBS}"');


    }
    # kdepimlibs variables
    if (! $in_kdelibs && ! $in_kdepimlibs) {
      $issues += 
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kabc[[:space:]]',
		   'replace "kabc" with "${KDE4_KABC_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kcal[[:space:]]',
		   'replace "kcal" with "${KDE4_KCAL_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]ktnef[[:space:]]',
		   'replace "ktnef" with "${KDE4_KTNEF_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kmime[[:space:]]',
		   'replace "kmime" with "${KDE4_KMIME_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kresources[[:space:]]',
		   'replace "kresources" with "${KDE4_KRESOURCES_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]emailfunctions[[:space:]]',
		   'replace "emailfunctions" with "${KDE4_EMAILFUNCTIONS_LIBS}"');
      $issues +=
        &checkLine($line,$linecnt,
                   'target_link_libraries.*[[:space:]]syndication[[:space:]]',
                   'replace "syndication" with "${KDE4_SYNDICATION_LIBS}"');
      $issues +=
        &checkLine($line,$linecnt,
                   'target_link_libraries.*[[:space:]]kldap[[:space:]]',
                   'replace "kldap" with "${KDE4_KLDAP_LIBS}"');	   
    }
  }

  #look for "missing" stuff
  my($in_exec)=0;
  my($has_project)=0;
  foreach $line (@lines) {
    chomp($line);
    $line =~ s/#.*$//; #remove comments
    next if ($line =~ m/^[[:space:]]$/); #skip blank lines
    $in_exec = 1
      if ($line =~ m/add_(|kdeinit_)executable[[:space:]]*\(/i); 
    if ($line =~ m/[Pp][Rr][Oo][Jj][Ee][Cc][Tt]/) {
      $has_project=1;
      last;
    }
  }
  if (! $has_project && $top_of_module && $in_exec) {
    $issues++;
    print "\tMissing a PROJECT() command\n";
  }

  close(IN);
  return $issues;
}

sub checkLine {
  my($line,$cnt,$regex,$explain) = @_;
  if ($line =~ m/$regex/) {
    print "\tline#$cnt: $explain\n";
    return 1;
  }
  return 0;
}

#==============================================================================
# Help function: print help message and exit.
sub Help {
  &Version();
  print "Check KDE CMakeLists.txt files for common errors.\n\n";
  print "Usage: $Prog [OPTIONS] FILES\n";
  print "  --help             display help message and exit\n";
  print "  --version          display version information and exit\n";
  print "\n";
  exit 0 if $help;
}

# Version function: print the version number and exit.
sub Version {
  print "$Prog, version $Version\n";
  exit 0 if $version;
}

__END__
