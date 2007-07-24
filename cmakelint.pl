#!/usr/bin/perl -w

###############################################################################
# Sanity checks CMakeLists.txt files.                                         #
# Copyright (C) 2006-2007 by Allen Winter <winter@kde.org>                    #
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
# You should have received a copy of the GNU General Public License along     #
# with this program; if not, write to the Free Software Foundation, Inc.,     #
# 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.               #
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
my($Version) = '1.5';

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
  my($in_kdebase)=0;
  $in_kdebase=1 if ($apath =~ m+/kdebase/+);
  my($top_of_module)=0;

  $top_of_module=1 if ($apath =~ m+/koffice/[a-zA-Z_1-9]*/CMakeLists.txt+);
  $top_of_module=1 if ($apath =~ m+/playground/[a-zA-Z_1-9]*/[a-zA-Z_1-9]*/CMakeLists.txt+); 
  $top_of_module=1 if ($apath =~ m+/extragear/[a-zA-Z_1-9]*/[a-zA-Z_1-9]*/CMakeLists.txt+);
  $top_of_module=1 if ($apath =~ m+/kde(libs|pimlibs|base|accessibility|addons|admin|artwork|bindings|edu|games|graphics|multimedia|network|pim|sdk|toys|utils|develop|devplatform|webdev)/[a-zA-Z_1-9]*/CMakeLists.txt+);
  $top_of_module=0 if ($apath =~ m+/(cmake|pics)/+);

  my(@lines) = <IN>;
  my($line);
  my($linecnt)=0;
  my($issues)=0;
  my(@ch,$c);
  my($nob,$ncb);
  my($nop,$ncp)=(0,0);
  #look for "bad" stuff
  foreach $line (@lines) {
    $linecnt++;
    chomp($line);
    $line =~ s/#.*$//; #remove comments

    next if (! $line);                   #skip empty lines
    next if ($line =~ m/^[[:space:]]$/); #skip blank lines

    @ch = split(//,$line);
    $nob = $ncb = 0;
    foreach $c (@ch) {
      $nop++ if ($c eq '(');
      $ncp++ if ($c eq ')');
      $nob++ if ($c eq '{');
      $ncb++ if ($c eq '}');
    }
    if ($nob != $ncb) {
      $issues++;
      print "\tline#$linecnt: Mismatched braces\n";
    }

    $issues += &checkLine($line,$linecnt,
			  '[[:space:]]{\$',
			  'replace "{$" with "${", or garbage detected');

    $issues += &checkLine($line,$linecnt,
			  '[[^:print:]]{\$',
			  'non-printable characters detected');

    $issues += &checkLine($line,$linecnt,
			  '[Kk][Dd][Ee]4_[Aa][Uu][Tt][Oo][Mm][Oo][Cc]',
			  'KDE4_AUTOMOC() is obsolete. Remove it.');

    $issues += &checkLine($line,$linecnt,
                          '^[[:space:]]*[Qq][Tt]4_[Aa][Uu][Tt][Oo][Mm][Oo][Cc]',
                          'No need for QT4_AUTOMOC(). Remove it.');

    $issues += &checkLine($line,$linecnt,
			  '[Kk][Dd][Ee]3_[Aa][Dd][Dd]_[Kk][Pp][Aa][Rr][Tt]',
			  'Use KDE4_ADD_PLUGIN() instead of KDE3_ADD_KPART()');

    $issues += &checkLine($line,$linecnt,
			  '^[[:space:]]*[Aa][Dd][Dd]_[Ll][Ii][Bb][Rr][Aa][Rr][Yy]',
			  'Use KDE4_ADD_LIBRARY() instead of ADD_LIBRARY()');

    $issues += &checkLine($line,$linecnt,
                          'DESTINATION[[:space:]]\${APPLNK_INSTALL_DIR}',
                          'APPLNK_INSTALL_DIR is dead with kde4 replace "${APPLNK_INSTALL_DIR}" with "${XDG_APPS_INSTALL_DIR}" and convert desktop file to xdg format (add Categories)');

    $issues += &checkLine($line,$linecnt,
                          'DESTINATION[[:space:]]\${MIME_INSTALL_DIR}',
                          'Files installed into MIME_INSTALL_DIR will not read. Port them on freedesktop xdg mimetypes.');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]/lib/kde[[:digit:]]',
			  'replace /lib/kde" with "${PLUGIN_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]lib',
			  'replace "lib" with "${LIB_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${LIB_INSTALL_DIR}/\$',
			  'replace "${LIB_INSTALL_DIR}/${...}" with "${LIB_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]/*include/*',
			  'replace "include" or "/include" with "${INCLUDE_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${INCLUDE_INSTALL_DIR}/\$',
			  'replace "${INCLUDE_INSTALL_DIR}/${...}" with "${INCLUDE_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]/*bin/*',
			  'replace "bin" or "/bin" with "${BIN_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${BIN_INSTALL_DIR}/\$',
			  'replace "${BIN_INSTALL_DIR}/${...}" with "${BIN_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
 			  'DESTINATION[[:space:]]/*share/apps/*',
 			  'replace "share/apps" or "/share/apps" with "${DATA_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
 			  'DESTINATION[[:space:]]\${DATA_INSTALL_DIR}/\$',
 			  'replace "${DATA_INSTALL_DIR}/${...}" with "${DATA_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
 			  'DESTINATION[[:space:]]/*share/applications/*',
 			  'replace "share/applications" or "/share/applications" with "${XDG_APPS_INSTALL_DIR}"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]/*share/autostart/*',
			  'replace "share/autostart" or "/share/autostart" with "${AUTOSTART_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${AUTOSTART_INSTALL_DIR}/\$',
			  'replace "${AUTOSTART_INSTALL_DIR}/${...}" with "${AUTOSTART_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]/*share/icons/*',
			  'replace "share/icons" or "/share/icons" with "${ICON_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${ICON_INSTALL_DIR}/\$',
			  'replace "${ICON_INSTALL_DIR}/${...}" with "${ICON_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]/*share/locale/*',
			  'replace "share/locale" or "/share/locale" with "${LOCALE_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${LOCALE_INSTALL_DIR}/\$',
			  'replace "${LOCALE_INSTALL_DIR}/${...}" with "${LOCALE_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]/*share/services/*',
			  'replace "share/services" or "/share/services" with "${SERVICES_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${SERVICES_INSTALL_DIR}/\$',
			  'replace "${SERVICES_INSTALL_DIR}/${...}" with "${SERVICES_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]/*share/sounds/*',
			  'replace "share/sounds" or "/share/sounds" with "${SOUND_INSTALL_DIR}"');
    $issues += &checkLine($line,$linecnt,
			  'DESTINATION[[:space:]]\${SOUND_INSTALL_DIR}/\$',
			  'replace "${SOUND_INSTALL_DIR}/${...}" with "${SOUND_INSTALL_DIR}/realname"');

    $issues += &checkLine($line,$linecnt,
                          'install_targets[[:space:]]*\(',
                          'replace "install_targets" with "install(TARGETS...)');

    $issues += &checkLine($line,$linecnt,
                          'INSTALL_TARGETS[[:space:]]*\(',
                          'replace "install_targets" with "install(TARGETS...)');
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
    $issues += &checkLine($line,$linecnt,
			  '-fexceptions',
			  'replace "-fexceptions" with "${KDE4_ENABLE_EXCEPTIONS}"');

      $issues +=
        &checkLine($line,$linecnt,
                   'set_target_properties.*PROPERTIES.*[[:space:]]VERSION[[:space:]][[:digit:]]',
                   'replace a hard-coded VERSION with "${GENERIC_LIB_VERSION}"');
      $issues +=
        &checkLine($line,$linecnt,
                   'set_target_properties.*PROPERTIES.*[[:space:]]SOVERSION[[:space:]][[:digit:]]',
                   'replace a hard-coded SOVERSION with "${GENERIC_LIB_SOVERSION}"');

    # kdelibs variables
    if (! $in_kdelibs) {
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kdeui[\s/)]',
		   'replace "kdeui" with "${KDE4_KDEUI_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kio[\s/)]',
		   'replace "kio" with "${KDE4_KIO_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kdefx[\s/)]',
		   'replace "kdefx" with "${KDE4_KDEFX_LIBS}"');
      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kdeprint[\s/)]',
            'replace "kdeprint" with "${KDE4_KDEPRINT_LIBS}"');
      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kdesu[\s/)]',
            'replace "kdesu" with "${KDE4_KDESU_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]khtml[\s/)]',
            'replace "khtml" with "${KDE4_KHTML_LIBS}"');
      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kparts[\s/)]',
            'replace "kparts" with "${KDE4_KPARTS_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kde3support[\s/)]',
            'replace "kde3support" with "${KDE4_KDE3SUPPORT_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kutils[\s/)]',
            'replace "kutils" with "${KDE4_KUTILS_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kdnssd[\s/)]',
            'replace "kdnssd" with "${KDE4_KDNSSD_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]knewstuff2[\s/)]',
            'replace "knewstuff2" with "${KDE4_KNEWSTUFF2_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]knotifyconfig[\s/)]',
            'replace "knotifyconfig" with "${KDE4_KNOTIFYCONFIG_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]threadweaver[\s/)]',
            'replace "threadweaver" with "${KDE4_THREADWEAVER_LIBRARIES}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]krosscore[\s/)]',
            'replace "krosscore" with "${KDE4_KROSSCORE_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]krossui[\s/)]',
            'replace "krossui" with "${KDE4_KROSSUI_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]phonon[\s/)]',
            'replace "phonon" with "${KDE4_PHONON_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kaudiodevicelist[\s/)]',
            'replace "kaudiodevicelist" with "${KDE4_KAUDIODEVICELIST_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]solidifaces[\s/)]',
            'replace "solidifaces" with "${KDE4_SOLIDIFACES_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]solid[\s/)]',
            'replace "solid" with "${KDE4_SOLID_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]ktexteditor[\s/)]',
            'replace "ktexteditor" with "${KDE4_KTEXTEDITOR_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kfile[\s/)]',
            'replace "kfile" with "${KDE4_KFILE_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]knepomuk[\s/)]',
            'replace "knepomuk" with "${KDE4_KNEPOMUK_LIBS}"');

      $issues +=
    &checkLine($line,$linecnt,
            'target_link_libraries.*[[:space:]]kmetadata[\s/)]',
            'replace "kmetadata" with "${KDE4_KMETADATA_LIBS}"');

    }

    # kebase variables
    if (! $in_kdelibs && ! $in_kdepimlibs && !$in_kdebase) {
      $issues +=
        &checkLine($line,$linecnt,
                   'target_link_libraries.*[[:space:]]plasma[\s/)]',
                   'replace "plasma" with "${PLASMA_LIBRARIES}"',
		   'add macro_optional_find_package(Plasma) in CMakeLists.txt');
   }

    # kdepimlibs variables
    if (! $in_kdelibs && ! $in_kdepimlibs) {
      $issues += 
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]gpgmepp[\s/)]',
		   'replace "gpgmepp" with "${KDE4_GPGMEPP_LIBS}"');
      $issues += 
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kabc[\s/)]',
		   'replace "kabc" with "${KDE4_KABC_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kblog[\s/)]',
		   'replace "kblog" with "${KDE4_KBLOG_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kcal[\s/)]',
		   'replace "kcal" with "${KDE4_KCAL_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kimap[\s/)]',
		   'replace "kimap" with "${KDE4_KIMAP_LIBS}"');
      $issues +=
        &checkLine($line,$linecnt,
                   'target_link_libraries.*[[:space:]]kldap[\s/)]',
                   'replace "kldap" with "${KDE4_KLDAP_LIBS}"');
       $issues +=
         &checkLine($line,$linecnt,
                    'target_link_libraries.*[[:space:]]kleo[\s/)]',
                    'replace "kleo" with "${KDE4_KLEO_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kmime[\s/)]',
		   'replace "kmime" with "${KDE4_KMIME_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kpimidentities[\s/)]',
		   'replace "kpimidentities" with "${KDE4_KPIMIDENTITIES_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kpimutils[\s/)]',
		   'replace "kpimutils" with "${KDE4_KPIMUTILS_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kresources[\s/)]',
		   'replace "kresources" with "${KDE4_KRESOURCES_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]ktnef[\s/)]',
		   'replace "ktnef" with "${KDE4_KTNEF_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]kxmlrpcclient[\s/)]',
		   'replace "kxmlrpcclient" with "${KDE4_KXMLRPCCLIENT_LIBS}"');
      $issues +=
        &checkLine($line,$linecnt,
                   'target_link_libraries.*[[:space:]]mailtransport[\s/)]',
                   'replace "mailtransport" with "${KDE4_MAILTRANSPORT_LIBS}"');
      $issues +=
	&checkLine($line,$linecnt,
		   'target_link_libraries.*[[:space:]]qgpgme[\s/)]',
		   'replace "qgpgme" with "${KDE4_QGPGME_LIBS}"');
      $issues +=
        &checkLine($line,$linecnt,
                   'target_link_libraries.*[[:space:]]syndication[\s/)]',
                   'replace "syndication" with "${KDE4_SYNDICATION_LIBS}"');
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
    print "\tline#$linecnt: Missing a PROJECT() command\n";
  }
  if ($nop != $ncp) {
    $issues++;
    print "\tline#$linecnt: Mismatched parens\n";
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
