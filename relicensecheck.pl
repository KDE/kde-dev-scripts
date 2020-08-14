#!/usr/bin/perl -w
# vim:sw=4:et
# (c) Dirk Mueller. GPLv2+
# I would love to be a python script, but os.popen just sucks

use strict;

use List::Util qw(any);

### Please add your KDE (svn/git) account name in *alphabetical* order to the list
### below, then answer the following questions:
###
### 1. Include 'gplv23' if you are okay with contributions you've made under
###    "GPLv2" being relicensed as "GPLv2 or GPLv3".
###
### 2. Include 'lgplv23' if you are okay with contributions you've made under
###    "LGPLv2" being relicensed as "LGPLv2 or LGPLv3".
###
### 3. Include 'gplv2+' if you are okay with contributions you've made under
###    "GPLv2" being relicensed as "GPLv2 or later".
###
### 4. Include 'lgplv2+' if you are okay with contributions you've made under
###    "LGPLv2" being relicensed as "LGPLv2 or later".
###
### 5. Include '+eV' if you are okay with the KDE e.V. deciding on a future
###    licensing change to your code if necessary.
###
### 5. Include 'CCBYSA4+' if you are okay with contributions you've made under
###    "GNU FDL" being relicensed as "Creative Commons Attribution-ShareAlike 4.0 International".
### For more information, see https://community.kde.org/Guidelines_and_HOWTOs/Relicensing/KDE_Relicensing

my %license_table = (
    'acrouthamel'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'aheinecke'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'ahmadsamir'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'arichardson'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'athurhfree'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'carlschwan'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'cfeck'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'cgerloff'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'cordlandwehr'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'davidedmundson'=> ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'davidhurka'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'davidre'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'emmanuelp'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'flherne'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'huoni'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'igorkushnir'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'jpoelen'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'jriddell'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'kezik'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'kleag'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', 'CCBYSA4' ],
    'lnj'           => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'ltoscano'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'lueck'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'mart'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'meven'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'mgallien'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'michaeleden'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'michelh'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'mjansen'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'muhlenpfordt'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'ngraham'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'nicolasfella'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'ostroffjh'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'rkflx'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'safaalfulaij'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'sitter'        => ['CCBYSA4'],
    'sredman'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'sstjames'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'tfella'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'thomassc'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'vladz'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ],
    'wbauer'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV', 'CCBYSA4' ]
);

my %old_license_table_2 = (
    # From before CCBYSA4 was added, if you get an update for one of these people move it to %license_table
    'aacid'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'abryant'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'adawit'        => ['gplv23', 'lgplv23',                      '+eV' ],
    'ademko'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'adiaferia'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'afiestas'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'alexmerry'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'alund'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'amantia'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'amth'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'andreyc'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'antlarr'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'apol'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'arnolddumas'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'asensi'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'aseigo'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'asserhal'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'beaulen'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bensi'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'beschow'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bgupta'        => [                     'gplv2+', 'lgplv2+', '+eV' ],
    'bhards'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bieker'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bischoff'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bks'           => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'blackie'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bport'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bram'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'broulik'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bruggie'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'bshah'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'capel'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'carewolf'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'cgiboudeaux'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'chani'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'chehrlic'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'clee'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'cmollekopf'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'coates'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'codrea'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'craig'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'cramblitt'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'cschumac'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'ctennis'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'cullmann'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'dakon'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'danimo'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'dannya'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'deller'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'denis'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'deniskuplyakov'=> ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'dfaure'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'dhaumann'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'dherberth'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'domi'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'djarvie'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'dyp'           => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'dvratil'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'egorov'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'ehamberg'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'eliasp'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'elvisangelaccio'=>['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'epignet'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'ereslibre'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'eros'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'ervin'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'eschepers'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+',       ],
    'eva'           => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'fabiank'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'fawcett'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'fengchao'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'fischer'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'fizz'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'flocati'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'fujioka'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'fux'           => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'garbanzo'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'gateau'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'geralds'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'giannaros'     => ['gplv23', 'lgplv23'                             ],
    'gioele'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'graesslin'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'granroth'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'gregormi'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'groszdaniel'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'grulich'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'guymaurel'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'haeber'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'haeckel'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+',       ],
    'harris'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'hausmann'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'heikobecker'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+',       ],
    'hein'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'hdhoang'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'hindenburg'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'hoelzer'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'hrvojes'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'hubner'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'huerlimann'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'huftis'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'ilic'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'ikomissarov'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'ivan'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'jbrouault'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'jehrichs'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'jekyllwu'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'jlee'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'johnflux'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'jones'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'jowenn'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'jschroeder'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'jtamate'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'kainhofe'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'kfunk'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'kloecker'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'knight'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'knauss'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'kossebau'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'kylafas'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'lbeltrame'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'leinir'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'leonh'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'leonhard'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'lilachaze'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'lliehu'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'lukas'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'lvsouza'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'lypanov'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'majewsky'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mardelle'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'martyn'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mbritton'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mbroadst'      => ['gplv23', 'lgplv23'                     , '+eV' ],
    'mecir'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'michaelhowell' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'michalhumpula' => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'milliams'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mirko'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mklapetek'     => ['gplv23', 'lgplv23',         ,          , '+eV' ],
    'mkoller'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mlaurent'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mludwig'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'mmrozowski'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mpyne'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mssola'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'mueller'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'mwolff'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'nalvarez'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'narvaez'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'nhasan'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'nikitas'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'nsams'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'ogoffart'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'orcsik'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'palant'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'palimaka'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'pdamsten'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'peifengyu'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'pgquiles'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'pino'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'pletourn'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'pupeno'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'raabe'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'rahn'          => ['gplv23', 'lgplv23'                     , '+eV' ],
    'ralfjung'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'ralsina'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'rdale'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'reiher'        => ['gplv23', 'lgplv23',                      '+eV' ],
    'rich'          => ['gplv23', 'lgplv23'                     , '+eV' ],
    'richih'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'rkcosta'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'robbilla'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'romariorios'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'rpreukschas'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'rthomsen'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'ruedigergad'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'sanders'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'sandsmark'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'sars'          => ['gplv23', 'lgplv23',                      '+eV' ],
    'saschpe'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'savernik'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'scarpino'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'schmeisser'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'schroder'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'schwarzer'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'sebas'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'skelly'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'smartins'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'sping'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'staikos'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'staniek'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'sune'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'taj'           => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'tanton'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'tenharmsel'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'teske'         => ['gplv23', 'lgplv23',                            ],
    'tfry'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'thiago'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'tjansen'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'tmcguire'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'tnyblom'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'treat'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'turbov'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'uga'           => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'uwolfer'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'vandenoever'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'vhanda'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'vitters'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'vkrause'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'vonreth'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'vrusu'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'waba'          => ['gplv23', 'lgplv23',                      '+eV' ],
    'wheeler'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+'        ],
    'whiting'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'willy'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'woebbe'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'wstephens'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'yurchor'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'zack'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ],
    'zecke'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+', '+eV' ]
    # Do not add anything here. Use license_table instead.
);

my %old_license_table = (
    ### below is the older table -- from before we offered the +eV option.
    ### This means that in theory some of these contributors might accept
    ### to add the +eV if we ask them nicely. If they refuse, move the line
    ### to the above part of the table so that we don't ask them again.
    'adridg'        => ['gplv23', 'lgplv23'                      ],
    'ahartmetz'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'annma'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'apaku'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'arendjr'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'aumuell'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'bbroeksema'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'binner'        => ['gplv23', 'lgplv23'                      ],
    'bjacob'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'bmeyer'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'boemann'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'borgese'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'braxton'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'bvirlet'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'cartman'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'cconnell'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'charles'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'cies'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'cniehaus'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'coolo'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'cschlaeg'      => ['gplv23', 'lgplv23'                      ],
    'dimsuz'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'djurban'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'dmacvicar'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'dymo'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'edghill'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'emmott'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'espen'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'fela'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'fredrik'       => ['gplv23', 'lgplv23'                      ],
    'gladhorn'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'gogolok'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'goossens'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'gyurco'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'harald'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'hedlund'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'helio'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'howells'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'hschaefer'     => ['gplv23'                                 ],
    'ingwa'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'isaac'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'jens'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'jlayt'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'johach'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'krake'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'laidig'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'lunakl'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'lure'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'marchand'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mattr'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mcamen'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'menard'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mfranz'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mhunter'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'micron'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mkretz'        => ['gplv23', 'lgplv23'                      ],
    'mlarouche'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mm'            => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mrudolf'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'msoeken'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mstocker'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mutz'          => [                     'gplv2+', 'lgplv2+' ],
    'mvaldenegro'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'mwoehlke'      => ['gplv23', 'lgplv23'                      ],
    'nielsslot'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'okellogg'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'onurf'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'orzel'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'ossi'          => [                     'gplv2+', 'lgplv2+' ],
    'osterfeld'     => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'pfeiffer'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'piacentini'    => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'pitagora'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'ppenz'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'pstirnweiss'   => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'putzer'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'pvicente'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'quique'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'raggi'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'rempt'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'rjarosz'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'rodda'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'roffet'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'rohanpm'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'sebsauer'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'shaforo'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'shipley'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'silberstorff'  => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'thorbenk'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'tilladam'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'tokoe'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'toma'          => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'troeder'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'trueg'         => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ], # NOTE: except k3b
    'wgreven'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'winterz'       => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'zachmann'      => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ],
    'zander'        => ['gplv23', 'lgplv23', 'gplv2+', 'lgplv2+' ]
    # Do not add anything here. Use license_table instead.
);

my %secondary_mail_addresses = (
    '13thguards@gmail.com' => 'isemenov',
    'ABBAPOH@me.com' => 'ikomissarov',
    'aetf@unlimitedcodeworks.xyz' => 'peifengyu',
    'aleixpol@kde.org' => 'apol',
    'alex.merry@kde.org' => 'alexmerry',
    'arichardson.kde@googlemail.com' => 'arichardson',
    'asmond@gmx.com' => 'asensi',
    'athurh@gmail.com' => 'athurhfree',
    'bero@arklinux.org' => 'bero',
    'caleb@gentoo.org' => 'ctennis',
    'christoph@maxiom.de' => 'cfeck',
    'devel@the-user.org' => 'schmidt-domine',
    'devriese@kde.org' => 'domi',
    'ervin+bluesystems@kde.org' => 'ervin',
    'faure+bluesystems@kde.org' => 'dfaure',
    'git@the-user.org' => 'schmidt-domine',
    'guy.maurel@kde.org' => 'guymaurel',
    'john@layt.net' => 'jlayt',
    'kde@randomguy3.me.uk' => 'alexmerry',
    'kde@rusu.info' => 'valir',
    'keziolio123@gmail.com' => 'kezik',
    'kubito@gmail.com' => 'rkcosta',
    'lamarque@gmail.com' => 'lvsouza',
    'majewsky@gmx.net' => 'majewsky',
    'malte@kde.org' => 'malte',
    'mk-lists@email.de' => 'kaning',
    'mklapetek@kde.org' => 'mklapetek',
    'neoclust.kde@free.fr' => 'nlecureuil',
    'olav@vitters.nl' => 'vitters',
    'richard@goteborg.utfors.se' => 'larkang',
    'schwarzerf@gmail.com' => 'schwarzer',
    'sune@vuorela.dk' => 'sune',
    'thb@net-bembel.de' => 'tbaumgart',
    'trev@adblockplus.org' => 'palant',
    'tsdgeos@terra.es' => 'aacid',
);

my %ruletable;
my %blacklist;
my %whitelist;
my %unknown_authors;
my @blacklist_revs;

foreach my $who (keys %old_license_table) {
    die "$who in both tables" if defined $license_table{$who};
    $license_table{$who} = $old_license_table{$who};
}

foreach my $who (keys %old_license_table_2) {
    die "$who in both tables" if defined $license_table{$who};
    $license_table{$who} = $old_license_table_2{$who};
}

foreach my $who (keys %license_table) {
    foreach my $license(@{$license_table{$who}}) {
        $ruletable{$license}->{$who} = 1;
    }
}

# Read kde-common/accounts for email->name mapping.

my $configfile = $ENV{HOME}. "/.config/KDE/relicensecheck.conf";

open(CONFIG, $configfile) or die "Please write the path to kde-common/accounts in $configfile";
my $accountfile;
while (<CONFIG>) {
    if (not /^#/) {
        chomp;
        $accountfile = $_;
    }
}
close CONFIG;
defined $accountfile or die "Please write the path to kde-common/accounts in $configfile";

my %authors = ();
my %authornames = ();
sub parseAccountsFile($)
{
    my ($accountfile) = @_;
    open(ACCOUNTS, $accountfile) || die "Account file not found: $accountfile";
    while (<ACCOUNTS>) {
        # The format is nick name email.
        if (/([^\s]*)\s+([^\s].*[^\s])\s+([^\s]+)/) {
            $authors{$3} = "$1";
            $authornames{$1} = "$2";
        }
        #elsif (/([^\s]*)\s+([^\s]*)/) {
        #    $authors{$1} = $2;
        #}
        else {
            die "$accountfile: couldn't parse $_";
        }
    }
    close ACCOUNTS;
}

if ($accountfile) {
    parseAccountsFile($accountfile);

    # Also read the "disabled accounts" file
    my $disabledaccountsfile = $accountfile;
    $disabledaccountsfile =~ s/accounts$/disabled-accounts/;
    die "I expected this to end with 'accounts': $accountfile" if ($accountfile eq $disabledaccountsfile);
    parseAccountsFile($disabledaccountsfile);
}

sub resolveEmail($) {
    my ($email) = @_;

    my $resolved = $authors{$email};
    if (not defined $resolved) {
        $resolved = $secondary_mail_addresses{$email};
    }
    if (not defined $resolved) {
        $unknown_authors{$email} = 1;
        return $email;
    }
    return $resolved;
}

sub skipCommitByAuthor($) {
    my ($author) = @_;
    return ($author eq "scripty" or
            $author eq "(no" or
            $author eq "nobody\@localhost" or
            $author eq "not.committed.yet" or
            $author eq "null\@kde.org");
}

sub usage()
{
 print << "EOM";
Usage:
    relicensecheck.pl file

    Output information on relicensing possibilities for <file>

    relicensecheck.pl -g
    relicensecheck.pl --generate-wiki

    Generate the table for the wiki page
EOM
}

my $generate_wiki = 0;
my @arguments;
sub parse_arguments(@)
{
    while (scalar @_) {
        my $arg = shift @_;

        if ($arg eq "-g" || $arg eq "--generate-wiki") {
            $generate_wiki = 1;
        } elsif ($arg eq "-?" || $arg eq "--?" || $arg eq "-h" || $arg eq "--help") {
            usage();
            exit 0;
        } elsif ($arg eq "--") {
            push @arguments, @_;
            return;
        } else {
            push @arguments, $arg;
        }
    }
}

parse_arguments(@ARGV);

if ($generate_wiki) {

    print "{| border=\"1\"\n";
    print "! Name !! GPLv2->GPLv2+ !! LGPLv2 -> LGPLv2+ !! GPLv2 -> GPLv2+v3 !! LGPLv2 -> LGPLv2+LGPLv3 !! KDE e.V. decides !! FDL -> CC-BY-SA 4.0 \n";
    print "|-\n";
    my @lines = ();
    foreach my $who (keys %license_table) {
        if (!defined $authornames{$who}) {
            die "ERROR: unknown author $who\n";
        }
        # Example: print "|Adam, Till || YES || YES || YES || YES || NO\n";
        my @licenses = @{$license_table{$who}};
        my %licensesHash = map { $_ => 1 } @licenses;
        my $gplv23 = exists($licensesHash{'gplv23'}) ? "YES" : "NO";
        my $lgplv23 = exists($licensesHash{'lgplv23'}) ? "YES" : "NO";
        my $gplv2plus = exists($licensesHash{'gplv2+'}) ? "YES" : "NO";
        my $lgplv2plus = exists($licensesHash{'lgplv2+'}) ? "YES" : "NO";
        my $eV = exists($licensesHash{'+eV'}) ? "YES" : "NO";
        $eV = "" if (exists $old_license_table{$who});
        my $ccbysa4 = exists($licensesHash{'CCBYSA4'}) ? "YES" : "NO";
        $ccbysa4 = "" if (exists $old_license_table_2{$who} || exists $old_license_table{$who});
        push @lines, "|$authornames{$who} || $gplv2plus || $lgplv2plus || $gplv23 || $lgplv23 || $eV || $ccbysa4\n";
    }
    use locale;
    foreach my $line (sort @lines) {
        print $line;
        print "|-\n";
    }
    print "|}\n";

    exit 0;
}
my $file = $arguments[0] || "";

die "need existing file: $file" if (! -r $file);

my $svn = (-d ".svn");

if ($svn) {
    open(IN, "-|") || exec 'svn', 'log', '-q', $file;
} else {
    # Format the git output to match the format of svn log.
    open(IN, "-|") || exec 'git', 'log', '--follow', '--abbrev-commit', '--pretty=format:r%h | %ae ', $file;
}
while(<IN>) {

    if (/^r(\S+) \| (\S+) /)  {
        my ($rev, $author) = ($1, $2);
        #print STDERR "rev=$rev author=$author\n";

        next if skipCommitByAuthor($author);

        if (not $svn) {
            # Resolve email to account name
            $author = resolveEmail($author);
        }

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
    if ($svn) {
        open(IN, "-|") || exec 'svn', 'ann', '-x', '-w', $file;
        while(<IN>) {
            my ($author) = (split)[1];
            $loc_author{$author}++;
        }
        close(IN);
    } else {
        open(IN, "-|") || exec 'git', 'blame', '-f', '-w', '-e', $file;
        while(<IN>) {
            # The format is:
            # b061712b kdecore/klockfile.cpp      (<faure@kde.org>   [...]
            if (m/^(\S+) (\S+) +\(<([^>]+)>/) {
                my ($author) = $3;
                next if skipCommitByAuthor($author);
                $author = resolveEmail($author);
                $loc_author{$author}++;
            } else {
                print STDERR "Parse error on git blame output: $_";
            }
        }
        close(IN);
    }
}

if (%unknown_authors) {
    print "The following emails do not appear in the accounts file:\n\n";
    foreach my $who(keys %unknown_authors) {
        print "$who\n";
    }
    print "\n";
}

if (defined (keys %blacklist)) {
    print "Need permission for licensing:\n\n";

    my %stat;

    foreach my $license(keys %blacklist) {
        print "- $license: ". join(' ', (keys %{$blacklist{$license}})) . "\n";
        foreach my $who(keys %{$blacklist{$license}}) {
            next if not defined $loc_author{$who};
            $stat{$license} += scalar(@{$blacklist{$license}->{$who}});
            printf "%9s (%4d LOC): %s \n", $who, $loc_author{$who} || 0, join(",", @{$blacklist{$license}->{$who}});
        }
        print "\n";
    }

    print "\n";
    print "Summary:\n";

    foreach my $license(sort { $stat{$a} <=> $stat{$b} } keys %stat) {
        printf "%5d commits preventing relicensing to %s\n", $stat{$license}, $license
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

print "\nDo not forget to check copyright headers and for patches committed in the name of others!\n";
