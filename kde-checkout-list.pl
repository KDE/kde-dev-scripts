#!/usr/bin/perl -w
# -*- cperl-indent-level: 2 -*-
###############################################################################
# Parses the KDE Projects XML Database and prints project protocol-url lines  #
# for each project in the specified component/module.                         #
#                                                                             #
# Copyright (C) 2011 by Allen Winter <winter@kde.org>                         #
# Copyright (C) 2011 by David Faure <faure@kde.org>                           #
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
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.                #
###############################################################################

# TODO
# - validation (once a DTD is available)

use strict;
use Getopt::Long;
use XML::Parser;
use LWP::Simple;		# used to fetch the xml db

my($Prog) = 'kde-checkout-list.pl';
my($Version) = '0.90';

my($help) = '';
my($version) = '';
my($searchComponent) = '';
my($searchModule) = '';
my($searchProtocol) = "git";
my($allmatches) = 0;
my($doClone) = 0;
my($doPrune) = 0;
my($dryRun) = 0;
my($gitSuffix) = 0;
my($branch) = '';

exit 1
if (!GetOptions('help' => \$help, 'version' => \$version,
		'component=s' => \$searchComponent,
		'module=s' => \$searchModule,
		'protocol=s' => \$searchProtocol,
                'all' => \$allmatches,
		'clone' => \$doClone,
		'prune' => \$doPrune,
		'dry-run' => \$dryRun,
		'gitsuffix' => \$gitSuffix,
		'branch=s' => \$branch
	       ));

&Help() if ($help);

if ($searchProtocol ne "git" &&
    $searchProtocol ne "http" &&
    $searchProtocol ne "ssh" &&
    $searchProtocol ne "tarball") {
  print "Invalid protocol \"$searchProtocol\" specified.\n";
  print "Run $Prog --help for more info\n";
  exit 1;
}
&Version() if ($version);

if ($searchModule && !$searchComponent) {
  print "Module specified, but not in which component. Please use --component kde for instance.\n";
  print "Run $Prog --help for more info\n";
  exit 1;
}

my $kdebranch='';
my $kdedash='';
if ($branch) {
  $kdebranch = "KDE/" . $branch;
  $kdedash = "kde-" . $branch;
}

my $curComponent = "";
my $curModule = "";
my $curProject = "";
my $curPath = "";
my $curUrl = "";
my $curActive = 1;
my $skipModule = 0;
my $inRepo = 0;
my $inPath = 0;
my $inUrl = 0;
my $inActive = 0;

my @element_stack;		# remember which elements are open
my %output;         # project name -> project data
my %projectByPath;  # project path -> project name

my $projects = get("http://projects.kde.org/kde_projects.xml");
die "Failed to download kde_projects.xml" unless defined $projects;

# sanity check
my @lines = split('\n',$projects);
if ($lines[0] !~ m/xml version/ || $lines[$#lines] !~ m+</kdeprojects>+) {
  print "The kde_projects.xml downloaded is invalid somehow. Try again\n";
  exit 1;
}

# initialize the parser
my $parser = XML::Parser->new( Handlers =>
			       {
				Start=>\&handle_start,
				End=>\&handle_end,
				Char=>\&char_handler,
				});

$parser->parse( $projects );

# print results
my($proj,$ret);
foreach $proj (sort keys %output) {
  if ( $output{$proj}{'active'} || $allmatches ) {
    my $subdir = $output{$proj}{'path'};
    my $url = $output{$proj}{'url'};
    print "$subdir $url\n";

    if ( $doClone ) {
      my $command;
      if ( ! -d "$subdir" ) {
#modules without the "KDE/" in the branchname are:
# kdebase/kate => only KDE/4.7 and above
# kdeexamples => No branches
# superbuild => No branches

	if ( $branch ) {
	  next if ( $subdir =~ m+/kdeexamples+ || $subdir =~ m+/superbuild+ );
	    $command = "git clone $url $subdir && cd $subdir && git checkout -b $kdebranch origin/$kdebranch";
	} else {
	  $command = "git clone $url $subdir";
	}
      } else {
	if ($branch) {
	  next if ( $subdir =~ m+/kdeexamples+ || $subdir =~ m+/superbuild+ );
	    $command = "cd $subdir && git config remote.origin.url $url && git checkout $kdebranch && git pull --ff";
	} else {
	  $command = "cd $subdir && git config remote.origin.url $url && git pull --ff";
	}
      }
      $ret = &runCommand( $command );
      if ($ret) {
	runCommand("rm -rf $subdir");
	printf "REMOVING CLONE DUE TO GIT FAILURE\n";
      }
    }
  }
}

# wipe out old checkouts, if requested
if ( $doPrune ) {
  my $startDir = ".";
  if ( $searchComponent ) {
    $startDir = $searchComponent;
    if ($branch) {
      my $foo = $searchComponent . "-" . $branch;
      $startDir =~ s+$searchComponent+$foo+;
    }
    if ( $searchModule ) {
      $startDir .= "/$searchModule";
    }
  }
  open(my $F, "find $startDir -name .git |");
  while (my $line = <$F>) {
    chomp $line;
    $line =~ s,/\.git,,;
    $line =~ s,^\./,,;
    if ( not exists $projectByPath{$line} ) {
      print STDERR "Deleting old git checkout: $line\n";
      runCommand( "rm -rf \"$line\"" );
    }
  }
}

sub runCommand {
  my ( $command ) = @_;
  my $ret = 0;
  if ( $dryRun ) {
    print STDERR "$command\n";
  } else {
    $ret = system( $command );
    $ret = $ret >> 8;
  }
  return $ret;
}

# process a start-of-element event: print message about element
#
sub handle_start {
  my( $expat, $element, %attrs ) = @_;

  # ask the expat object about our position
  my $line = $expat->current_line;

  # remember this element and its starting position by pushing a
  # little hash onto the element stack
  push( @element_stack, { element=>$element, line=>$line });

  #print STDERR "-- $element\n";

  if ( $element eq "component" ) {
    my $value = $attrs{"identifier"};
    #print STDERR "component identifier=$value\n";
    if ( (!$searchComponent or ($value eq $searchComponent)) ) {
      $curComponent = $value;
      $curModule = "";
      $curProject = "";
      $inRepo = 0;

      #print STDERR "BEGIN component $curComponent.\n";
    }
  }

  if ( $curComponent && $element eq "module" ) {
    my $value = $attrs{"identifier"};
    $curProject = "";
    if ( !$searchModule or ($value eq $searchModule) ) {
      $curModule = $value;
      #print STDERR "BEGIN module $curModule\n";
      $skipModule = 0;
    } else {
      $skipModule = 1;
      #print STDERR "SKIP module $value\n";
    }
  }

  if ( $curComponent && !$skipModule && $element eq "project" ) {
    $curProject = $attrs{"identifier"};
    if (!$curModule) {
      print STDERR "project without a module! $curProject\n";
    }
    #print STDERR "BEGIN project $curProject\n";
  }

  if ($curComponent && !$skipModule) {
    if ( $element eq "path" ) {
      $inPath = 1;
    } elsif ( $element eq "repo" ) {
      $inRepo = 1;
      $curActive = 1; # assume all repos are active by default
    } elsif ( $inRepo && $element eq "url" ) {
      my $value = $attrs{"protocol"};
      if ( $value eq $searchProtocol ) {
	$inUrl = 1;
      }
    } elsif ( $inRepo && $element eq "active" ) {
      $inActive = 1;
    }
  }
}

# process an end-of-element event
#
sub handle_end {
  my( $expat, $element ) = @_;

  # We'll just pop from the element stack with blind faith that
  # we'll get the correct closing element, since XML::Parser will scream
  # bloody murder if any well-formedness errors creep in.
  my $element_record = pop( @element_stack );

  if ( $element eq "component" && $curComponent ) {
    #print "END of component $curComponent\n";
    $curComponent = "";
  }
  if ( $element eq "module" && $curComponent && $curModule ) {
    #print "END of module $curModule\n";
    $curModule = "";
  }
  if ( $element eq "project" && $curComponent && $curModule && $curProject ) {
    #print "END of project $curProject\n";
    $curProject = "";
    $curUrl = "";
  }
  if ( $element eq "repo" && $curComponent && $inRepo ) {
    #print STDERR "repo in $curPath: $curUrl\n";
    $inRepo = 0;
    if ( $curUrl && $curPath ) {
      if ($branch) {
        my $foo = $curComponent . "-" . $branch;
        $curPath =~ s+$curComponent+$foo+;
      }
      my $subdir = $curPath;
      $curPath .= "-git" if ($gitSuffix && -d "$curPath/.svn");
      # $subdir is the logical name (extragear/network/konversation)
      # while $curPath is the physical path (extragear/network/konversation-git)
      $output{$subdir}{'path'} = $curPath;
      $output{$subdir}{'url'} = $curUrl;
      $output{$subdir}{'active'} = $curActive;
      $projectByPath{$curPath} = $subdir;
    } else {
      if (!$curUrl) {
	print STDERR "ERROR: repo without url! $curComponent $curModule $curProject $curPath\n";
      } elsif (!$curPath) {
	print STDERR "ERROR: repo without path! $curComponent $curModule $curProject $curUrl\n";
      }
    }
  }
  if ( $element eq "path" ) {
    $inPath = 0;
  } elsif ( $element eq "url" ) {
    $inUrl = 0;
  } elsif ( $element eq "active" ) {
    $inActive = 0;
  }
}

sub char_handler
{
  my ($p, $data) = @_;

  $data =~ s/\n/\n\t/g;
  if ( $inPath ) {
    $curPath = $data;
  } elsif ( $inUrl ) {
    $curUrl = $data;
  } elsif ( $inActive ) {
    $curActive = !( $data =~ m/false/i || $data =~ m/off/i );
  }

}  # End of default_handler

# Help function: print help message and exit.
sub Help {
  &Version();
  print "Parses the KDE Projects XML Database and prints project protocol-url lines\n";
  print "for each project in the specified component/module.\n\n";
  print "  --help        display help message and exit\n";
  print "  --version     display version information and exit\n";
  print "  --component   search for projects within this component\n";
  print "  --module      search for projects within this module (requires --component)\n";
  print "  --branch      git checkout the specified branch, i.e. 4.6\n";
  print "  --protocol    print the URI for the specified protocol (default=\"git\")\n";
  print "                possible values are \"git\", \"http\", \"ssh\" or \"tarball\"\n";
  print "  --all         print all projects, not just active-only projects\n";
  print "\n";
  print "  --clone       actually do a git clone or pull of every repo found\n";
  print "      Note: this is meant for servers like lxr/ebn rather than for developers.\n";
  print "  --gitsuffix   append '-git' to the directory name when cloning, if a svn dir exists.\n";
  print "  --prune       remove old git checkouts that are not listed anymore\n";
  print "  --dry-run     show git and prune commands but don't execute them.\n";
  print "\n";
  print "Examples:\n\n";
  print "To print the active projects in extragear network with git protocol:\n";
  print "% $Prog --component=extragear --module=network\n";
  print "\n";
  print "To print all projects in playground utils with the ssh protocol:\n";
  print "% $Prog --component=playground --module=utils --protocol=ssh --all\n";
  print "\n";
  exit 0 if $help;
}

# Version function: print the version number and exit.
sub Version {
  print "$Prog, version $Version\n";
  exit 0 if $version;
}
