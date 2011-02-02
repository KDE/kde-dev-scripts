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
my($Version) = '0.80';

my($help) = '';
my($version) = '';
my($searchComponent) = '';
my($searchModule) = '';
my($searchProtocol) = "git";
my($allmatches) = 0;
my($doClone) = 0;
my($gitSuffix) = 0;

exit 1
if (!GetOptions('help' => \$help, 'version' => \$version,
		'component=s' => \$searchComponent,
		'module=s' => \$searchModule,
		'protocol=s' => \$searchProtocol,
                'all' => \$allmatches,
		'clone' => \$doClone,
		'gitsuffix' => \$gitSuffix
	       ));

&Help() if ($help);

if ($searchProtocol ne "git" &&
    $searchProtocol ne "http" &&
    $searchProtocol ne "ssh" &&
    $searchProtocol ne "tarball") {
  print "Invalid protocol \"$searchProtocol\" specified.\n";
  print "Run $Prog --help for more info\n";
  exit 0;
}
&Version() if ($version);

my $curComponent = "";
my $curModule = "";
my $curProject = "";
my $curUrl = "";
my $curActive = 1;
my $skipModule = 0;
my $inRepo = 0;
my $inUrl = 0;
my $inActive = 0;

my @element_stack;		# remember which elements are open
my %output;

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
my($proj);
foreach $proj (sort keys %output) {
  if ( $output{$proj}{'active'} || $allmatches ) {
    my $subdir = $output{$proj}{'name'};
    my $url = $output{$proj}{'url'};
    print "$subdir $url\n";

    if ( $doClone ) {
      $subdir = $subdir . "-git" if ($gitSuffix && -d "$subdir/.svn");
      if ( ! -d "$subdir" ) {
	system( "git clone $url $subdir" ); # error handling? don't want to abort though
      } else {
	system( "cd $subdir && git config remote.origin.url $url && git pull --ff" );
      }
    }
  }
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

  if ( $element eq "component" ) {
    my $value = $attrs{"identifier"};
    #print STDERR "component identifier=$value\n";
    if ( (!$searchComponent or ($value eq $searchComponent)) ) {
      $curComponent = $value;
      $curModule = "";
      $curProject = "";

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
    }
  }

  if ( $curComponent && $element eq "project" ) {
    $curProject = $attrs{"identifier"};
    if (!$curModule) {
      print STDERR "project without a module! $curProject\n";
    }
    #print STDERR "BEGIN project $curProject\n";
  }

  if ( $inRepo && $element eq "url" ) {
    my $value = $attrs{"protocol"};
    if ( $value eq $searchProtocol ) {
      $inUrl = 1;
    }
  }

  if ( $element eq "repo" && !$skipModule ) {
    $inRepo = 1;
    $curActive = 1; # assume all repos are active by default
  }
  if ( $inRepo && $element eq "active" ) {
    $inActive = 1;
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
  }
  if ( $element eq "repo" && $curComponent && $inRepo ) {
    $inRepo = 0;
    if ( $curUrl ) {
      my $guy;
      if ( !$curProject ) {
	if (!$curModule) {
	  $guy = $curComponent;
	  #print STDERR "component $guy\n";
	} else {
	  $guy = "$curComponent/$curModule";
	  #print STDERR "module $guy\n";
	}
      } else {
	#if ($moduleless) {
	#  $guy = "$curComponent/$curProject";
	#} else {
	  if (!$curModule) {
	    print STDERR "ERROR: component $curComponent project $curProject, no module!\n";
	  }
	  $guy = "$curComponent/$curModule/$curProject";
	#}
	#print STDERR "project $guy\n";
      }
      $output{$guy}{'name'} = $guy;
      $output{$guy}{'url'} = $curUrl;
      $output{$guy}{'active'} = $curActive;
    }
  }
  if ( $element eq "url" && $inRepo ) {
    $inUrl = 0;
  }
  if ( $element eq "active" && $inRepo && $curComponent && $curModule && $curProject && $inRepo ) {
    $inActive = 0;
  }
}

sub char_handler
{
  my ($p, $data) = @_;

  $data =~ s/\n/\n\t/g;
  if ( $inUrl ) {
    $curUrl = $data;
  }
  if ( $inActive ) {
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
  print "  --protocol    print the URI for the specified protocol (default=\"git\")\n";
  print "                possible values are \"git\", \"http\", \"ssh\" or \"tarball\"\n";
  print "  --all         print all projects, not just active-only projects\n";
  print "\n";
  print "  --clone       actually do a git clone or pull of every repo found\n";
  print "      Note: this is meant for servers like lxr/ebn rather than for developers.\n";
  print "  --gitsuffix       append '-git' to the directory name when cloning, if a svn dir exists.\n";
# TODO print" --prune       remove old git checkouts that are not listed anymore\n";
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
