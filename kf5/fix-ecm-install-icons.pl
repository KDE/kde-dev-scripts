#!/usr/bin/perl -w

# Fix Deprecation Warnings from ECM_INSTALL_ICONS
# when using the old form
# ecm_install_icons(<icon_install_dir>)
#
# Usage: ./fix-ecm-install-icons.pl <CMake Lists file>

use strict;

use File::Basename;
use File::Slurp::Unicode;
use File::Find;

#TODO Figure out a portable way to do this
my $path_separator = "/";

sub findGitRepo {
    my $dir = dirname(shift);
    
    while (!-e "$dir/.git" ) {
	$dir = dirname($dir);
    }

    return $dir;
}

#TODO figure prefix for targets using either repo name or arg

foreach my $cmakelists (@ARGV) {
    my $cmakecontents = read_file($cmakelists);
    my $cwdir = dirname($cmakelists);
    my $repodir = findGitRepo($cmakelists);
    my $targetname = $cwdir;

    $targetname =~ s/^$repodir//;
    $targetname =~ s/$path_separator/_/g;

    chdir($cwdir);

    if ($cmakecontents =~ /ecm_install_icons\s*\(\s*([^\s]+)\s*\)/) {
	my $destination = $1;
	my $replacestr = "";
	my %themehash = ();
	
	find(sub {
	    if ($_ =~ /(br|ox|cr|lo|hi)(\d\d|sc)-(\w+)-(\w+).(png|svgz|mng)/) {
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

		$themehash{$th} .= "\n\t$newfilename";
	        `git mv $_ $newfilename`;
	    }
	     }, $cwdir);

	foreach my $key (sort keys %themehash) {
	    $replacestr .= "ecm_install_icons(ICONS$themehash{$key}\n\tDESTINATION $destination\n\tTHEME ";

	    if ($key eq "br") {
		$replacestr .=  "breeze";
	    }
	    elsif ($key eq "ox") {
		$replacestr .= "oxygen"
	    }
	    elsif ($key eq "cr") {
		$replacestr .= "crystalsvg"
	    }
	    elsif ($key eq "lo") {
		$replacestr .= "locolor"
	    }
	    elsif ($key eq "hi") {
		$replacestr .= "hicolor"
	    }

	    $replacestr .= "\n)\n\n";
	}

	chop($replacestr);
	
	if ($replacestr ne "") {
	    $cmakecontents =~ s/ecm_install_icons\s*\(\s*[^\s]+\s*\)/$replacestr/;

	    write_file($cmakelists, $cmakecontents);
	    `git add $cmakelists`;
	}
    }
}

system("git", "status");
