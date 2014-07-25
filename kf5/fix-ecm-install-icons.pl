#!/usr/bin/perl -w

# Fix Deprecation Warnings from ECM_INSTALL_ICONS
# when using the old form
# ecm_install_icons(<icon_install_dir>)
#
# Usage: ./fix-ecm-install-icons.pl [--indent indentstr] [CMakeLists.txt files]
# You can use '\t' escapes in the indentstr. The default is four spaces.
# If no files are given, CMakeLists.txt from the current directory is used.
#
# NB: if your CMakeLists.txt uses kde4_install_icons, you should
#     run the adapt_cmakelists_file.pl script first

use strict;

use File::Basename;
use File::Slurp::Unicode;
use File::Find;
use Cwd;

sub findGitRepo {
    my $dir = dirname(shift);

    while (not $dir eq '/' and !-e "$dir/.git") {
        $dir = dirname($dir);
    }

    if ($dir eq '/') {
        return "";
    }
    return $dir;
}

my $indent = '    ';

if ((scalar(@ARGV) > 0) and (($ARGV[0] eq '--indent') or ($ARGV[0] eq '-i'))) {
    shift @ARGV;
    if (scalar(@ARGV) eq 0) {
        print "No indent string given\n";
        exit 1;
    }
    $indent = $ARGV[0];
    shift @ARGV;
    $indent =~ s/\\t/\t/g;
    if (not $indent =~ /^\s*$/) {
        print "Indent string is not whitespace\n";
        exit 2;
    }
}

if (scalar(@ARGV) eq 0) {
    @ARGV = ("CMakeLists.txt");
}

my $lastfile = "";

my $savedir = getcwd;

foreach my $cmakelists (@ARGV) {
    my $mv = 'mv';
    my $cmakecontents = read_file($cmakelists);
    my $cmakelistsfile = basename($cmakelists);
    my $cwdir = dirname($cmakelists);
    my $repodir = findGitRepo($cmakelists);
    if ($repodir) {
        $mv = 'git mv';
    }

    chdir($cwdir);

    if ($cmakecontents =~ /ecm_install_icons\s*\(\s*([^\s]+)(?:\s+([^\s]+))?\s*\)/) {
        my $destination = $1;
        my $l10n_code = $2;
        my $replacestr = "";
        my %themehash = ();

        foreach my $file (<{*.png,*.svgz,*.mng}>) {
            if ($file =~ /(br|ox|cr|lo|hi)(\d\d|sc)-(\w+)-([^\.]+)\.(png|svgz|mng)/) {
                my $th = $1;
                my $size = $2;
                my $group = $3;
                my $iconname = $4;
                my $extension = $5;

                if ($group eq "mime") {
                    $group = "mimetypes";
                }
                elsif ($group eq "filesys") {
                    $group = "places";
                }
                elsif ($group eq "device") {
                    $group = "devices";
                }
                elsif ($group eq "app") {
                    $group = "apps";
                }
                elsif ($group eq "action") {
                    $group = "actions";
                }

                my $newfilename = "$size-$group-$iconname.$extension";

                $themehash{$th} .= "\n${indent}$newfilename";
                `$mv "$file" "$newfilename"`;
            }
        }

        foreach my $key (sort keys %themehash) {
            $replacestr .= "ecm_install_icons(ICONS$themehash{$key}\n";
            $replacestr .= "${indent}DESTINATION $destination\n";
            $replacestr .= "${indent}THEME ";

            if ($key eq "br") {
                $replacestr .=  "breeze";
            }
            elsif ($key eq "ox") {
                $replacestr .= "oxygen";
            }
            elsif ($key eq "cr") {
                $replacestr .= "crystalsvg";
            }
            elsif ($key eq "lo") {
                $replacestr .= "locolor";
            }
            elsif ($key eq "hi") {
                $replacestr .= "hicolor";
            }
            $replacestr .= "\n";

            if ($l10n_code) {
                $replacestr .= "${indent}LANG $l10n_code\n";
            }

            $replacestr .= ")\n";
        }

        chop($replacestr);

        if ($replacestr ne "") {
            $cmakecontents =~ s/ecm_install_icons\s*\(\s*[^\s]+(?:\s+[^\s]+)?\s*\)/$replacestr/;

            write_file($cmakelistsfile, $cmakecontents);
            if ($repodir) {
                `git add $cmakelistsfile`;
            }
        }
    }

    $lastfile = $cmakelists;

    # so relative paths work on the next iteration
    chdir($savedir);
}

# Hopefully all of these files were in the same repo :)
my $repodir = findGitRepo($lastfile);
chdir($repodir);
system("git", "status");
# vim:et:sw=4:sts=4:
