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
    'binner',
    'bmeyer',
    'boemann',
    'cconnell',
    'chani',
    'coolo',
    'craig',
    'cschlaeg',
    'cschumac',
    'cullmann',
    'danimo',
    'dfaure',
    'dmacvicar',
    'dymo',
    'edghill',
    'ereslibre',
    'ervin',
    'espen',
    'fredrik',
    'granroth',
    'hausmann',
    'ingwa',
    'jens',
    'johnflux',
    'jriddell',
    'rodda',
    'kainhofe',
    'knight',
    'laidig',
    'lunakl',
    'lypanov',
    'martyn',
    'mattr',
    'mbroadst',
    'mkretz',
    'mlaurent',
    'mpyne',
    'mueller',
    'osterfeld',
    'pino',
    'raabe',
    'rich',
    'rohanpm',
    'thiago',
    'tilladam',
    'tmcguire',
    'trueg',
    'uwolfer',
    'waba',
    'winterz',
    'woebbe',
    'wstephens',
) {
    $whitelist{"gplv23"}->{$who} = 1;
}

foreach my $who(
    'amantia',
    'apaku',
    'aseigo',
    'binner',
    'bmeyer',
    'boemann',
    'cconnell',
    'chani',
    'coolo',
    'craig',
    'cschlaeg',
    'cschumac',
    'cullmann',
    'danimo',
    'dfaure',
    'dmacvicar',
    'dymo',
    'edghill',
    'ereslibre',
    'ervin',
    'espen',
    'fredrik',
    'granroth',
    'hausmann',
    'ingwa',
    'jens',
    'johnflux',
    'jriddell',
    'rodda',
    'kainhofe',
    'knight',
    'laidig',
    'lunakl',
    'lypanov',
    'martyn',
    'mattr',
    'mbroadst',
    'mkretz',
    'mlaurent',
    'mpyne',
    'mueller',
    'osterfeld',
    'pino',
    'raabe',
    'rich',
    'rohanpm',
    'thiago',
    'tilladam',
    'tmcguire',
    'trueg',
    'uwolfer',
    'waba',
    'winterz',
    'woebbe',
    'wstephens',
) {
    $whitelist{"lgplv23"}->{$who} = 1;
}

foreach my $who(
    'apaku',
    'aseigo',
    'bmeyer',
    'boemann',
    'cconnell',
    'chani',
    'codrea',
    'coolo',
    'craig',
    'cschumac',
    'danimo',
    'dfaure',
    'dmacvicar',
    'dymo',
    'edghill',
    'ereslibre',
    'ervin',
    'espen',
    'granroth',
    'hausmann',
    'ingwa',
    'jens',
    'johnflux',
    'jriddell',
    'rodda',
    'kainhofe',
    'knight',
    'laidig',
    'lunakl',
    'lypanov',
    'martyn',
    'mattr',
    'mlaurent',
    'mpyne',
    'mueller',
    'mutz',
    'ossi',
    'osterfeld',
    'pino',
    'rohanpm',
    'thiago',
    'tilladam',
    'tmcguire',
    'trueg',
    'uwolfer',
    'winterz',
    'woebbe',
    'wstephens',
) {
    $whitelist{"gplv2+"}->{$who} = 1;
}

foreach my $who(
    'apaku',
    'aseigo',
    'bmeyer',
    'boemann',
    'cconnell',
    'chani',
    'codrea',
    'coolo',
    'craig',
    'cschumac',
    'dfaure',
    'dmacvicar',
    'dymo',
    'edghill',
    'ereslibre',
    'ervin',
    'espen',
    'granroth',
    'hausmann',
    'ingwa',
    'jens',
    'johnflux',
    'jriddell',
    'rodda',
    'kainhofe',
    'knight',
    'laidig',
    'lunakl',
    'lypanov',
    'martyn',
    'mattr',
    'mlaurent',
    'mpyne',
    'mueller',
    'mutz',
    'ossi',
    'osterfeld',
    'pino',
    'rohanpm',
    'thiago',
    'tilladam',
    'tmcguire',
    'trueg',
    'uwolfer',
    'winterz',
    'woebbe',
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

my %loc_author = ();

if (-f $file) {
    open(IN, "-|") || exec 'svn', 'ann', $file;
    while(<IN>) {
        my ($author) = (split)[1];
        $loc_author{$author}++;
    }
    close(IN);
}

if (defined (keys %blacklist)) {
    print "Need permission for licensing:\n\n";

    my %stat;

    foreach my $license(keys %blacklist) {
        print "- $license:\n";
        foreach my $who(keys %{$blacklist{$license}}) {
            $stat{$license} += length(@{$blacklist{$license}->{$who}});
            printf "%9s (%4d LOC): %s \n", $who, $loc_author{$who} || 0, join(",", @{$blacklist{$license}->{$who}});
        }
        print "\n";
    }

    print "\n";
    print "Summary:\n";

    foreach my $license(sort { $stat{$a} <=> $stat{$b} } keys %stat) {
        printf "%5d commits possibly violating %s\n", $stat{$license}, $license
    }
}
