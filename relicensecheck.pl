#!/usr/bin/perl -w
# vim:sw=4:et
# (c) Dirk Mueller. GPLv2+
# I would love to be a python script, but os.popen just sucks

use strict;

my %ruletable;
my %blacklist;
my %whitelist;
my @blacklist_revs;

# licensing table.

my %license_table = (
    'aacid'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'adawit'    => ['gplv23', 'lgplv23',                      '+eV' ],
    'amth'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'aseigo'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'asserhal'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'beaulen'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bieker'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bischoff'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bks'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bram'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bruggie'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'cramblitt' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'chehrlic'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'deller'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'denis'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'dfaure'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'danimo'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'dyp'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'ereslibre' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'eros'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'eschepers' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+',       ],
    'eva'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'fawcett'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'fujioka'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'haeber'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'haeckel'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+',       ],
    'hdhoang'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'hoelzer'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'garbanzo'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'giannaros' => ['gplv23', 'lgplv23'                             ],
    'ilic'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'jbrouault' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'jlee'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'jones'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'jowenn'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'kloecker'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'kossebau'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mbritton'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
 'michaelhowell'=> ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mirko'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mlaurent'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mpyne'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mueller'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'nhasan'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'ogoffart'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'pdamsten'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'pino'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'pletourn'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'pupeno'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'raabe'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'reiher'    => ['gplv23', 'lgplv23',                      '+eV' ],
    'robbilla'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'sanders'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'taj'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'tenharmsel'=> ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'teske'     => ['gplv23', 'lgplv23',                            ],
    'tmcguire'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'uga'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'vkrause'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'waba'      => ['gplv23', 'lgplv23',                      '+eV' ],
    'wheeler'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'whiting'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'willy'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'woebbe'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'zack'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'zecke'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],

    ### below is the older table -- from before we offered the +eV option.
    ### This means that in theory some of these contributors might accept
    ### to add the +eV if we ask them nicely. If they refuse, move the line
    ### to the above part of the table so that we don't ask them again.
    'adridg'    => ['gplv23', 'lgplv23'                      ],
    'ahartmetz' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'alexmerry' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'amantia'   => ['gplv23', 'lgplv23'                      ],
    'annma'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'apaku'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'arendjr'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'aumuell'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'binner'    => ['gplv23', 'lgplv23'                      ],
    'bjacob'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'bmeyer'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'boemann'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'borgese'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'bram'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'braxton'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'bvirlet'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'cartman'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'cconnell'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'chani'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'charles'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'chehrlic'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'cies'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'clee'      => ['gplv23', 'lgplv23'                      ],
    'cniehaus'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'codrea'    => [                     'gplv2+', 'lgplv2+' ],
    'coolo'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'craig'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'cschlaeg'  => ['gplv23', 'lgplv23'                      ],
    'cschumac'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'cullmann'  => ['gplv23', 'lgplv23'                      ],
    'dannya'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'dimsuz'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'djurban'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'dmacvicar' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'dymo'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'edghill'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'emmott'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'epignet'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'ereslibre' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'ervin'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'espen'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'fela'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'fizz'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'fredrik'   => ['gplv23', 'lgplv23'                      ],
    'gladhorn'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'gogolok'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'goossens'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'granroth'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'gyurco'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'hausmann'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'harald'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'harris'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'hedlund'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'helio'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'howells'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'hschaefer' => ['gplv23'                                 ],
    'huerlimann'=> ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'ilic'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'ingwa'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'isaac'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'jens'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'jlayt'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'johach'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'johnflux'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'jriddell'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'rodda'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'raggi'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'rjarosz'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'kainhofe'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'kleag'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'knight'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'krake'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'laidig'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'lunakl'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'lure'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'lypanov'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'marchand'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'martyn'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mattr'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mbroadst'  => ['gplv23', 'lgplv23'                      ],
    'mcamen'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'menard'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mfranz'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mhunter'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'micron'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'milliams'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mkretz'    => ['gplv23', 'lgplv23'                      ],
    'mlarouche' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mm'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mrudolf'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'msoeken'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mstocker'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mueller'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mutz'      => [                     'gplv2+', 'lgplv2+' ],
   'mvaldenegro'=> ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mwoehlke'  => ['gplv23', 'lgplv23'                      ],
    'nielsslot' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'ogoffart'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'okellogg'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'onurf'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'orzel'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'ossi'      => [                     'gplv2+', 'lgplv2+' ],
    'osterfeld' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'pfeiffer'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'piacentini'=> ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'pitagora'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'ppenz'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
   'pstirnweiss'=> ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'putzer'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'pvicente'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'quique'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'raabe'     => ['gplv23', 'lgplv23'                      ],
    'rahn'      => ['gplv23', 'lgplv23'                      ],
    'ralsina'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'rich'      => ['gplv23', 'lgplv23'                      ],
    'rempt'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'roffet'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'rohanpm'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'schmeisser'=> ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'schwarzer' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'sebsauer'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'shaforo'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'shipley'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
  'silberstorff'=> ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'thiago'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'thorbenk'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'tilladam'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'tmcguire'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'toma'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'tokoe'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'treat'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'troeder'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'trueg'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'uwolfer'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'wgreven'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'winterz'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'wstephens' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'zachmann'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'zander'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ]
    # Do not add anything here. Use the first half of the table.
);

foreach my $who (keys %license_table) {
    foreach my $license(@{$license_table{$who}}) {
        $ruletable{$license}->{$who} = 1;
    }
}

my $file = $ARGV[0] || "";

die "need existing file: $file" if (! -r $file);

open(IN, "-|") || exec 'svn', 'log', '-q', $file;
while(<IN>) {

    if (/^r(\d+) \| (\S+) /)  {
        my ($rev, $author) = ($1, $2);

        next if ($author eq "scripty" or $author eq "(no");

        foreach my $license(keys %ruletable) {
            if (!defined($ruletable{$license}->{$author})) {
                push(@{$blacklist{$license}->{$author}}, $rev);
            }
            else {
                push(@{$whitelist{$license}->{$author}}, $rev);
            }
 
        }
    }
}
close(IN);

my %loc_author = ();

if (-f $file) {
    open(IN, "-|") || exec 'svn', 'ann', '-x', '-w', $file;
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

my @allowed_list = ();

if (defined (keys %whitelist)) {
    foreach my $license(keys %whitelist) {
        next if defined($blacklist{$license});
        push(@allowed_list, $license);
    }
}

if ($#allowed_list >= 0) {
    print "\nRelicensing allowed: ". join(' ', @allowed_list) . "\n";
}

print "\ndo not forget to check copyright headers and for patches committed in the name of others!\n";
