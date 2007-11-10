#!/usr/bin/perl -w
# vim:sw=4:et
# (c) Dirk Mueller. GPLv2+
# I would love to be a python script, but os.popen just sucks

use strict;

my %whitelist;
my %blacklist;
my @blacklist_revs;

foreach my $who(
    'amantia',
    'apaku',
    'aseigo',
    'coolo',
    'craig',
    'cschlaeg',
    'cullmann',
    'dfaure',
    'danimo',
    'dymo',
    'espen',
    'fredrik',
    'granroth',
    'jens',
    'johnflux',
    'kainhofe',
    'knight',
    'lypanov',
    'martyn',
    'mlaurent',
    'mueller',
    'rich',
    'trueg',
    'waba',
    'wstephens',
) {
    $whitelist{"gplv23"}->{$who} = 1;
}

foreach my $who(
    'amantia',
    'apaku',
    'aseigo',
    'coolo',
    'craig',
    'cschlaeg',
    'cullmann',
    'dfaure',
    'danimo',
    'dymo',
    'espen',
    'fredrik',
    'granroth',
    'jens',
    'johnflux',
    'kainhofe',
    'knight',
    'lypanov',
    'martyn',
    'mlaurent',
    'mueller',
    'rich',
    'trueg',
    'waba',
    'wstephens',
) {
    $whitelist{"lgplv23"}->{$who} = 1;
}

foreach my $who(
    'aseigo',
    'apaku',
    'coolo',
    'craig',
    'dfaure',
    'danimo',
    'dymo',
    'espen',
    'granroth',
    'jens',
    'johnflux',
    'kainhofe',
    'knight',
    'lypanov',
    'martyn',
    'mlaurent',
    'mueller',
    'mutz',
    'trueg',
    'wstephens',
) {
    $whitelist{"gplv2+"}->{$who} = 1;
}

foreach my $who(
    'apaku',
    'aseigo',
    'coolo',
    'craig',
    'dfaure',
    'dymo',
    'espen',
    'granroth',
    'jens',
    'johnflux',
    'kainhofe',
    'knight',
    'lypanov',
    'martyn',
    'mlaurent',
    'mueller',
    'mutz',
    'trueg',
    'wstephens',
) {
    $whitelist{"lgplv2+"}->{$who} = 1;
}

my $file = $ARGV[0] || "";

die "need existing file: $file" if (! -r $file);

open(IN, "-|") || exec 'svn', 'log', '-q', $file;
while(<IN>) {

    if (/^r(\d+) \| (\S+) /)  {
        my ($rev, $author) = ($1, $2);

        next if ($author eq "scripty" or $author eq "(no");

        foreach my $license(keys %whitelist) {
            if (!defined($whitelist{$license}->{$author})) {
                push(@{$blacklist{$license}->{$author}}, $rev);
            }
        }
    }
}
close(IN);

if (defined (keys %blacklist)) {
    print "Need permission for licensing:\n\n";

    my %stat;

    foreach my $license(keys %blacklist) {
        print "- $license:\n";
        foreach my $who(keys %{$blacklist{$license}}) {
            $stat{$license} += length(@{$blacklist{$license}->{$who}});
            printf "%8s:  %s\n", $who, join(",", @{$blacklist{$license}->{$who}});
        }
        print "\n";
    }

    print "\n";
    print "Summary:\n";

    foreach my $license(sort { $stat{$a} <=> $stat{$b} } keys %stat) {
        printf "%5d commits possibly violating %s\n", $stat{$license}, $license
    }
}
