#! /usr/bin/env perl

# CORRECTIONS GO IN THE __DATA__ SECTION AT THE END OF THIS SCRIPT

# Checks and corrects common spelling errors in text files - code
# derived from kde-spellcheck.pl (Dirk Mueller <mueller@kde.org>)
#
# Copyright (c) 2004 Richard Evans <rich@ridas.com>
#
# License: LGPL 2.0
#
# 2004-05-14: Richard Evans <rich@ridas.com>
#
# Initial revision differs from kde-spellcheck.pl as follows:
# 
# Text file detection no longer spawns external program.
# No longer checks cwd if arguments omitted - just specify '.'
# No longer recurses through sub directories without --recurse.
# Can now check internal dictionary for mistakes using aspell.
# Changes are not made unless --make-changes is specified.
# File modification now uses an intermediate file to reduce the
# chance of data loss.
# Fixed bug that missed words with apostrophes.
# Removed the code checking for "nonword misspelling" - I don't
# believe it was doing anything useful, but please correct me
# if that's not the case!
# Corrected some dictionary entries.
# Runs much, much faster.

sub usage
{
  warn <<"EOF";

kde-spellcheck.pl [flags] filenames/directories

This script checks for, and optionally replaces, commonly misspelled words
with the correct US English equivalents. The behaviour has changed from
kde-spellcheck.pl - to check subdirectories you must specify --recurse,
omitting arguments does not check the current directory, and changes are
not made to files unless you specify --make-changes

CAUTION IS NEEDED WHEN USING THIS SCRIPT - changes are made to the original
file and are not programming language syntax aware - this is why the script
only suggests the changes to be made unless --make-changes is specified.

Hidden files, CVS directories, .desktop, and .moc files are excluded
from checking.

--check-dictionary    : Checks the internal dictionary for potential
                        spelling mistakes - requires aspell with a US
                        English dictionary, and Text::Aspell installed.

--suggest-corrections : Behaves as --check-dictionary, but also suggests
                        spelling corrections.

--recurse             : Check subdirectories recursively.
--quiet               : Disable informational output (not recommended).
--make-changes        : Displays the changes that would have been made.
--help|?              : Display this summary.

EOF

  exit;
}

use strict;
use warnings;
use Getopt::Long;
use File::Temp qw( tempfile );
use File::Copy qw( copy );

my $DICTIONARY = build_dictionary_lookup_table();

###########################################################################################
# Add options here as necessary - perldoc Getopt::Long for details on GetOptions

die "kde-spellcheck2 --help for usage\n"
      unless GetOptions ( "check-dictionary"    => \my $opt_check_dictionary,
                          "suggest-corrections" => \my $opt_suggest_corrections,
                          "quiet"               => \my $opt_quiet,
                          "make-changes"        => \my $opt_make_changes,
                          "recurse"             => \my $opt_recurse,
                          "help|?"              => \&usage );

check_dictionary($opt_suggest_corrections) if $opt_suggest_corrections or $opt_check_dictionary;

usage() unless @ARGV;

require File::MMagic;

my $MIME = File::MMagic->new;

my @dirqueue;

$opt_quiet = 0 unless $opt_make_changes;

sub info; *info = $opt_quiet ? sub {} : sub { print @_ };

for ( @ARGV )
{
  if    ( -f   ) { check_file($_)     }
  elsif ( -d _ ) { push @dirqueue, $_ }
  else           { warn "Unknown: '$_' is neither a directory or file" }
}

my $dir;

process_directory($dir) while $dir = pop @dirqueue;

$opt_make_changes or print <<EOF;

NB No changes have been made to any file. Please check the output to
see if the suggested changes are correct - if so, re-run this script
adding argument --make-changes 

EOF

###########################################################################################

sub check_file
{
  my $filename = shift;
  my $fh;

  unless ( open $fh, "<", $filename )
  {
    warn "Failed to open: '$filename': $!";
    return;
  }

  my $file_modified = 0;
  my @contents      = <$fh>;

  close $fh or warn "Failed to close: '$filename': $!";

  my $original;
  my $line_no = 0;
            
  for ( @contents )
  {
    $line_no++;
    $original = $_ unless $opt_make_changes;
                                                                  
    for my $word ( split /[^\w']/ )    # \W would split on apostrophe
    {
      next unless defined (my $correction = $DICTIONARY->{$word});

      $file_modified ||= 1;

      s/\b$word\b/$correction/g;
      
      info "$filename ($line_no): $word => $correction\n";
    }

    print "FROM: $original",
          "  TO: $_\n"        if !$opt_make_changes and $_ ne $original;
  }

  return unless $file_modified;
  return unless $opt_make_changes;

  info "Correcting: $filename\n";

  my ($tmp_fh, $tmp_filename) = tempfile(UNLINK => 0);

  eval
  {
    print $tmp_fh @contents or die "Write";

    $tmp_fh->flush          or die "Flush";
    $tmp_fh->seek(0, 0)     or die "Seek";
  };

  die "$@ failed on: '$tmp_filename': $!" if $@;

  copy($tmp_fh, $filename) or die "Failed to copy: $tmp_filename => $filename: $!\n", 
                                  "You can manually restore from: $tmp_filename";

  close  $tmp_fh       or warn  "Close failed on: '$tmp_filename': $!";
  unlink $tmp_filename or warn "Unlink failed on: '$tmp_filename': $!";
}


# Could be more robustly rewitten with File::Find / File::Find::Rules etc

sub process_directory
{
  my $directory = shift;

  info "Processing directory: $directory\n";

  opendir my $dh, $directory or die "Failed to read dir: '$directory': $!";

  while ( my $entry = readdir($dh) )
  {
    if ( $entry =~ /^\./        or
         $entry =~ /\.desktop$/ or
         $entry =~ /\.moc$/     or
         $entry eq "CVS" )
    {
      info "Skipping excluded file or directory: $entry\n";
      next;
    }

    my $file = "$directory/$entry";

    if ( -d $file )
    {
      push(@dirqueue, $file) if $opt_recurse;
      next;
    }

    next unless -f _;

    # First use perl's heuristic check to discard files as quickly as possible...

    unless ( -T _ )
    {
      info "Skipping binary file: $file\n";
      next;
    }

    # ...it's not always good enough though, so now check the Mimetype

    unless ( (my $type = $MIME->checktype_filename($file)) =~ /text/i )
    {
      info "Skipping $type file: $file\n";
      next;
    }

    check_file($file);
  }

  closedir $dh or warn "Failed to close dir: '$directory': $!";
}


########################################################################################################

sub check_dictionary
{
  my $suggest_corrections = shift;

  print <<EOF;

Attempting to check the internal dictionary - you must have aspell
and the perl module Text::Aspell installed for this to succeed,
otherwise the script will fail at this point.

EOF

  require Text::Aspell;

  my $speller = Text::Aspell->new or die "Failed to create Text::Aspell instance";

  # Despite the docs, set_option doesnt seem to return undef on error ...

  $speller->set_option('lang','en_US')
    or die "Text::Aspell failed to select language: 'en_US'", $speller->errstr; 

  # ... so try a very simple check

  unless ( $speller->check('color') )
  {
    warn "You dont appear to have the en_US dictionary installed - cannot check";
    exit;
  }

  print "Checking Lexicon for identical misspelling and corrections:\n";

  while ( my($key, $value) = each %$DICTIONARY )
  {
    print "\n$key" if $key eq $value;
  }

  print "\n\nChecking Lexicon for possible misspellings:\n\n";

  for my $word ( values %$DICTIONARY )
  {
    next if $speller->check($word);

    print "$word\n";
    print join(", ", $speller->suggest($word)), "\n\n" if $suggest_corrections;
  }

  print "\n";

  exit;
}


########################################################################################################

sub build_dictionary_lookup_table
{
  my %hash;

  while (<DATA>)
  {
    next if /^\s*$/ or /^\s*#/;   # Skip blank lines and comments

    next unless /^\s*"([^"]+)"\s+(.*)\s*$/ or /^\s*(\S+)\s+(.*)\s*$/;

    if ( $1 eq $2 )
    {
      warn "WARNING: Ignoring identical misspelling and correction: '$1' in __DATA__ offset line $.\n";
      next;
    }

    $hash{$1} = $2;
  }

  return \%hash;
}

__DATA__

#INCORRECT SPELLING    CORRECTION

aasumes                assumes
abailable              available
Abbrevation            Abbreviation
abbrevations           abbreviations
abbriviate             abbreviate
abbriviation           abbreviation
abilties               abilities
Ablolute               Absolute
abreviate              abbreviate
acces                  access
accesible              accessible
accesing               accessing
accomodate             accommodate
accross                across
Acess                  Access
achive                 achieve
achived                achieved
achiving               achieving
acknoledged            acknowledged
acknowledgement        acknowledgment
Acknowledgements       Acknowledgments
Acknowlege             Acknowledge
acommodate             accommodate
aconyms                acronyms
acording               according
acount                 account
acouting               accounting
activ                  active
actons                 actions
acually                actually
adapater               adapter
adatper                adapter
addded                 added
adddress               address
Additinoally           Additionally
additionaly            additionally
Additionaly            Additionally
additionnal            additional
additonal              additional

#INCORRECT SPELLING    CORRECTION

Addtional              Additional
aditional              additional
adminstrator           administrator
Adminstrator           Administrator
adress                 address
Adress                 Address
adressed               addressed
adresses               addresses
advertize              advertise
aesthetic              esthetic
Afganistan             Afghanistan
agressive              aggressive
Agressive              Aggressive
agressively            aggressively
alignement             alignment
alligned               aligned
Allignment             Alignment
allmost                almost
allready               already
allways                always
Allways                Always
alook                  a look
alot                   a lot
alows                  allows
alrady                 already
alreay                 already
alternativly           alternatively
ammount                amount
Ammount                Amount
analagous              analogous
analizer               analyzer
analogue               analog
analyse                analyze
analyses               analyzes
anfer                  after
angainst               against
annoucement            announcement
announcments           announcements
anwer                  answer
anwser                 answer
anwsers                answers
aplication             application
appeareance            appearance
appearence             appearance
appeares               appears
apperarance            appearance
appers                 appears

#INCORRECT SPELLING    CORRECTION

applicaiton            application
Applicalble            Applicable
appliction             application
appplication           application
approciated            appreciated
appropiate             appropriate
approriate             appropriate
approximatly           approximately
apropriate             appropriate
aquire                 acquire
aquired                acquired
arbitarily             arbitrarily
arbitary               arbitrary
Arbitary               Arbitrary
aribrary               arbitrary
aribtrarily            arbitrarily
aribtrary              arbitrary
arround                around
assosciated            associated
assosiated             associated
assoziated             associated
asssembler             assembler
assumend               assumed
asume                  assume
asynchonous            asynchronous
asyncronous            asynchronous
aticles                articles
atleast                at least
atomicly               atomically
attatchment            attachment
auhor                  author
authentification       authentication
authoratative          authoritative
authorisations         authorizations
automaticaly           automatically
Automaticaly           Automatically
automaticly            automatically
autoreplacment         autoreplacement
auxilary               auxiliary
Auxilary               Auxiliary
avaible                available
Avaible                Available
availble               available
availibility           availability
availible              available
Availible              Available
avaliable              available
avaluate               evaluate
avare                  aware
aviable                available
backrefences           backreferences
baloon                 balloon
basicly                basically

#INCORRECT SPELLING    CORRECTION

Basicly                Basically
beautifull             beautiful
becuase                because
beeep                  beep
beeing                 being
beexported             be exported
befor                  before
beggining              beginning
begining               beginning
behaviour              behavior
Behaviour              Behavior
Belarussian            Belarusian
beteen                 between
betrween               between
betweeen               between
Blueish                Bluish
bofore                 before
botton                 bottom
boudaries              boundaries
boundries              boundaries
boundry                boundary
boxs                   boxes
bruning                burning
buton                  button
Buxfixes               Bugfixes
cacheing               caching
calulation             calculation
cancelation            cancellation
cancelled              canceled
cancelling             canceling
capabilites            capabilities
caracters              characters
cataloge               catalog
Cataloge               Catalog
catalogue              catalog
catched                caught
ceneration             generation
centralised            centralized
centre                 center
Centre                 Center
changable              changeable
chaning                changing
characers              characters
charachters            characters
Characteres            Characters
charakters             characters
charater               character
Chatacter              Character
chatwindow             chat window
childs                 children
choosed                chose
choosen                chosen
Choosen                Chosen

#INCORRECT SPELLING    CORRECTION

chosing                choosing
cirumstances           circumstances
classess               classes
cloumn                 column
Coffie                 Coffee
colaboration           collaboration
collecion              collection
collumns               columns
coloum                 column
coloumn                column
colour                 color
colours                colors
colum                  column
comamnd                command
comination             combination
commense               commence
commerical             commercial
comming                coming
commited               committed
commiting              committing
Commiting              Committing
commmand               command
commuication           communication
communcation           communication
compability            compatibility
comparision            comparison
Comparision            Comparison
comparisions           comparisons
Compatability          Compatibility
compatibilty           compatibility
compatiblity           compatibility
Compedium              Compendium
compiiled              compiled
compleion              completion
completly              completely
complient              compliant
comsumer               consumer
comunication           communication
concatonated           concatenated
concurent              concurrent
configration           configuration
Configuraton           Configuration
connent                connect
connnection            connection
consecutivly           consecutively
consequtive            consecutive
constuctors            constructors
contactlist            contact list
containg               containing
contexual              contextual
contigious             contiguous
contingous             contiguous
continouos             continuous

#INCORRECT SPELLING    CORRECTION

continous              continuous
Continous              Continuous
contiribute            contribute
contoller              controller
Contorll               Control
controler              controller
controling             controlling
controll               control
conver                 convert
convient               convenient
convinience            convenience
conviniently           conveniently
coordiator             coordinator
Copys                  Copies
coresponding           corresponding
corrent                correct
correponds             corresponds
Costraints             Constraints
Coudn't                Couldn't
coursor                cursor
Coverted               Converted
coypright              copyright
cricles                circles
criticisim             criticism
cryptograhy            cryptography
Culculating            Calculating
curren                 current
currenty               currently
curteousy              courtesy
Custimize              Customize
customisation          customization
customise              customize
Customise              Customize
customised             customized
cutsom                 custom
cutt                   cut
Cutt                   Cut
datas                  data
DCOPCient              DCOPClient
deactive               deactivate
Deamon                 Daemon
debuging               debugging
Debuging               Debugging
decriptor              descriptor
defaul                 default
defered                deferred
Defininition           Definition
defintions             definitions
deleteing              deleting
Demonsrative           Demonstrative
Denstiy                Density
depencies              dependencies

#INCORRECT SPELLING    CORRECTION

dependeds              depends
dependend              dependent
dependig               depending
depricated             deprecated
derfined               defined
derivs                 derives
descide                decide
desciptor              descriptor
descryption            description
desctroyed             destroyed
desiabled              disabled
desidered              desired
desination             destination
deskop                 desktop
desription             description
Desription             Description
destiantion            destination
determiend             determined
determins              determines
detremines             determines
develloped             developed
developerss            developers
developped             developed
devided                divided
devide                 divide
diabled                disabled
diable                 disable
diaglostic             diagnostic
dialag                 dialog
dialler                dialer
Dialler                Dialer
dialling               dialing
Dialling               Dialing
dialogue               dialog
diaog                  dialog
didnt                  didn't
diffcult               difficult
differenciate          differentiate
differenly             differently
Differntiates          Differentiates
dificulty              difficulty
Difusion               Diffusion
digitised              digitized
diplayed               displayed
dirctely               directly
dirctory               directory
direcory               directory
directorys             directories
directoy               directory
disactivate            deactivate
disappers              disappears
Disbale                Disable

#INCORRECT SPELLING    CORRECTION

discontigous           discontiguous
discpline              discipline
discription            description
disppear               disappear
dissassembler          disassembler
distingush             distinguish
distribtuion           distribution
distrubutor            distributor
divizor                divisor
docucument             document
documentaiton          documentation
documentors            documenters
doens't                doesn't
doesnt                 doesn't
donnot                 do not
Donot                  Do not
dont                   don't
dont't                 don't
Dou                    Do
draging                dragging
dreamt                 dreamed
Droped                 Dropped
duotes                 quotes
durring                during
dynamicly              dynamically
eallocate              deallocate
eample                 example
editory                editor
efficent               efficient
efficently             efficiently
effiency               efficiency
embedabble             embeddable
embedable              embeddable
embeddabble            embeddable
embeded                embedded
emcompass              encompass
emty                   empty
encyption              encryption
enhandcements          enhancements
enles                  endless
enought                enough
entitities             entities
entrys                 entries
Entrys                 Entries
enumarated             enumerated
envirnment             environment
envirnoment            environment
enviroment             environment
environemnt            environment
environent             environment
Equador                Ecuador
equiped                equipped
equlas                 equals

#INCORRECT SPELLING    CORRECTION

errorous               erroneous
errror                 error
escriptor              descriptor
espacially             especially
espesially             especially
Evalute                Evaluate
everytime              every time
exacly                 exactly
exapmle                example
excecpt                except
execeeded              exceeded
execess                excess
exection               execution
execuable              executable
executeble             executable
exept                  except
exisiting              existing
existance              existence
exlusively             exclusively
exmaple                example
experienceing          experiencing
explicitely            explicitly
explicity              explicitly
explit                 explicit
Expresion              Expression
expresions             expressions
extented               extended
extention              extension
Extention              Extension
extentions             extensions
extesion               extension
fabilous               fabulous
falg                   flag
familar                familiar
fastes                 fastest
favourable             favorable
favour                 favor
favourite              favorite
favours                favors
featue                 feature
feeded                 fed
filsystem              filesystem
firware                firmware
fisrt                  first
fixiated               fixated
fixiate                fixate
fixiating              fixating
flaged                 flagged
flavours               flavors
focussed               focused
folllowed              followed
follwing               following
folowing               following

#INCORRECT SPELLING    CORRECTION

Folowing               Following
footnotexs             footnotes
formaly                formally
fortunatly             fortunately
foward                 forward
fragement              fragment
framesyle              framestyle
framset                frameset
fucntion               function
Fucntion               Function
fuction                function
fuctions               functions
fufill                 fulfill
fulfiling              fulfilling
fullfilled             fulfilled
funcion                function
funciton               function
functin                function
funtional              functional
funtionality           functionality
funtion                function
funtions               functions
furthur                further
gaalxies               galaxies
Gamee                  Game
gernerated             generated
ges                    goes
Ghostscipt             Ghostscript
giude                  guide
globaly                globally
goind                  going
Gostscript             Ghostscript
grapphis               graphics
greyed                 grayed
guaranted              guaranteed
guarenteed             guaranteed
guarrantee             guarantee
gziped                 gzipped
handeling              handling
harware                hardware
Harware                Hardware
hasnt                  hasn't
havn't                 haven't
heigt                  height
heigth                 height
hiddden                hidden
Hierachical            Hierarchical
highlighlighted        highlighted
highligting            highlighting
Higlighting            Highlighting
honour                 honor
honouring              honoring

#INCORRECT SPELLING    CORRECTION

honours                honors
horziontal             horizontal
hypens                 hyphens
hysical                physical
iconized               iconified
illumnating            illuminating
imaginery              imaginary
i'm                    I'm
imitatation            imitation
immedialely            immediately
immediatly             immediately
imortant               important
imperical              empirical
implemantation         implementation
implemenation          implementation
implenetation          implementation
implimention           implementation
implmentation          implementation
inactiv                inactive
incldue                include
incomming              incoming
Incomming              Incoming
incovenient            inconvenient
indeces                indices
indentical             identical
Indentification        Identification
indepedancy            independency
independant            independent
independend            independent
indetectable           undetectable
indicdate              indicate
indice                 index
indictes               indicates
infinitv               infinitive
infomation             information
informaion             information
informatation          information
informationon          information
informations           information
Inifity                Infinity
inital                 initial
initalization          initialization
initalized             initialized
initalize              initialize
Initalize              Initialize
initialisation         initialization
initialise             initialize
initialising           initializing
Initialyze             Initialize
Initilialyze           Initialize
initilization          initialization
initilize              initialize

#INCORRECT SPELLING    CORRECTION

Initilize              Initialize
innacurate             inaccurate
innacurately           inaccurately
insde                  inside
inteface               interface
interactivelly         interactively
interfer               interfere
interfrace             interface
interisting            interesting
internationalisation   internationalization
interrrupt             interrupt
interrumped            interrupted
interrups              interrupts
Interupt               Interrupt
intervall              interval
intervalls             intervals
intiailize             initialize
Intial                 Initial
intialisation          initialization
intialization          initialization
intialize              initialize
Intialize              Initialize
intializing            initializing
introdutionary         introductory
introdution            introduction
intrrupt               interrupt
intruction             instruction
invarient              invariant
invokation             invocation
Ionisation             Ionization
irrevesible            irreversible
isntance               instance
is'nt                  isn't
issueing               issuing
istory                 history
Iterface               Interface
itselfs                itself
journalised            journalized
judgement              judgment
kdelbase               kdebase
keyboad                keyboard
klicking               clicking
knowlege               knowledge
Konquerer              Konqueror
konstants              constants
kscreensave            kscreensaver
labelling              labeling
Labelling              Labeling
lauching               launching
layed                  laid
learnt                 learned
leats                  least
lenght                 length

#INCORRECT SPELLING    CORRECTION

Lenght                 Length
Licenced               Licensed
licence                license
Licence                License
Licens                 License
liset                  list
listenening            listening
listveiw               listview
litle                  little
litteral               literal
localisation           localization
losely                 loosely
maanged                managed
maching                matching
magnication            magnification
magnifcation           magnification
mailboxs               mailboxes
maillinglists          mailinglists
maintainance           maintenance
maintainence           maintenance
Malicous               Malicious
mamage                 manage
managment              management
Managment              Management
manangement            management
mannually              manually
Mantainer              Maintainer
manupulation           manipulation
marbels                marbles
matchs                 matches
maximimum              maximum
Maxium                 Maximum
mdification            modification
mdified                modified
menues                 menus
mesages                messages
messanger              messenger
messanging             messaging
Microsft               Microsoft
millimetres            millimeters
mimimum                minimum
minimise               minimize
minimising             minimizing
Minimun                Minimum
Minium                 Minimum
minumum                minimum
miscelaneous           miscellaneous
miscelanous            miscellaneous
miscellaneaous         miscellaneous
miscellanous           miscellaneous
Miscellanous           Miscellaneous
mispeled               misspelled
mispelled              misspelled

#INCORRECT SPELLING    CORRECTION

mistery                mystery
Modifes                Modifies
modifing               modifying
modul                  module
mosue                  mouse
Mozzila                Mozilla
mssing                 missing
Mulitimedia            Multimedia
mulitple               multiple
multible               multiple
multipe                multiple
multy                  multi
mutiple                multiple
neccesary              necessary
neccessary             necessary
necessery              necessary
nedd                   need
neet                   need
negativ                negative
negociated             negotiated
negociation            negotiation
neighbourhood          neighborhood
neighbour              neighbor
Neighbour              Neighbor
neighbours             neighbors
neogtiation            negotiation
nessecarry             necessary
nessecary              necessary
nessesary              necessary
nework                 network
newtork                network
nickanme               nickname
nonexistant            nonexistent
noone                  nobody
Noone                  No-one
normalisation          normalization
noticable              noticeable
nucleous               nucleus
obtail                 obtain
occoured               occurred
occouring              occurring
occurance              occurrence
occurances             occurrences
occured                occurred
occurence              occurrence
occurences             occurrences
occure                 occur
occuring               occurring
occurrance             occurrence
occurrances            occurrences
ocupied                occupied
offical                official
ommited                omitted

#INCORRECT SPELLING    CORRECTION

onthe                  on the
opend                  opened
optimisation           optimization
optionnal              optional
orangeish              orangish
organisational         organizational
organisation           organization
Organisation           Organization
organisations          organizations
organised              organized
organise               organize
organiser              organizer
organising             organizing
Organising             Organizing
orginate               originate
Originaly              Originally
orignal                original
oscilating             oscillating
otehr                  other
ouput                  output
outputing              outputting
overidden              overridden
overriden              overridden
overriden              overridden
ownes                  owns
pakage                 package
panelised              panelized
paramaters             parameters
parametres             parameters
parametrize            parameterize
paramter               parameter
paramters              parameters
particip               participle
particularily          particularly
paticular              particular
Pendings               Pending
percetages             percentages
Perfomance             Performance
performace             performance
Periferial             Peripheral
permision              permission
permisions             permissions
permissable            permissible
Personalizsation       Personalization
perticularly           particularly
phyiscal               physical
plaforms               platforms
plese                  please
politness              politeness
posibilities           possibilities
posibility             possibility

#INCORRECT SPELLING    CORRECTION

posible                possible
positon                position
possebilities          possibilities
possebility            possibility
possibilty             possibility
possiblity             possibility
posssibility           possibility
potentally             potentially
practise               practice
practising             practicing
preceeded              preceded
preceeding             preceding
precison               precision
preemphasised          preemphasized
Preemphasised          Preemphasized
prefered               preferred
Prefered               Preferred
preferrable            preferable
prefiously             previously
preformance            performance
prerequisits           prerequisites
presense               presence
pressentation          presentation
prgramm                program
Prining                Printing
priveleges             privileges
priviledge             privilege
priviledges            privileges
priviliges             privileges
probatility            probability
probelm                problem
proberly               properly
problmes               problems
proceedure             procedure
proctection            protection
proecss                process
progess                progress
programing             programming
programme              program
programm               program
promille               per mill
promiscous             promiscuous
promped                prompted
pronounciation         pronunciation
Pronounciation         Pronunciation
pronunce               pronounce
pronunciated           pronounced
properies              properties
Propertites            Properties
Propogate              Propagate
protoypes              prototypes

#INCORRECT SPELLING    CORRECTION

Proxys                 Proxies
psuedo                 pseudo
Psuedo                 Pseudo
pult                   desk
purposees              purposes
quatna                 quanta
queing                 queuing
querys                 queries
queueing               queuing
Queueing               Queuing
quiten                 quiet
quiting                quitting
readony                readonly
realise                realize
realy                  really
REAMDE                 README
reasonnable            reasonable
receieve               receive
recepeient             recipient
recepient              recipient
recevie                receive
recevie                receive
receving               receiving
recieved               received
recieve                receive
Recieve                Receive
reciever               receiver
recieves               receives
Recieves               Receives
recives                receives
recognised             recognized
recognise              recognize
recognises             recognizes
recomended             recommended
recommanded            recommended
recommand              recommend
recommented            recommended
redialling             redialing
reets                  resets
refered                referred
Refering               Referring
Refeshes               Refreshes
refreshs               refreshes
regarless              regardless
registaration          registration
registred              registered
Regsiter               Register
regulare               regular
regularily             regularly
Reigster               Register
reimplemenations       reimplementations

#INCORRECT SPELLING    CORRECTION

Reimplemenations       Reimplementations
releated               related
relection              reselection
relevent               relevant
relocateable           relocatable
remaing                remaining
remeber                remember
remebers               remembers
remotley               remotely
renderes               renders
renewd                 renewed
reorienting            reorientating
Repalcement            Replacement
replys                 replies
reponsibility          responsibility
requeusts              requests
resently               recently
resetted               reset
resistent              resistant
resognized             recognized
resonable              reasonable
resoure                resource
responsability         responsibility
responsivness          responsiveness
resposible             responsible
ressources             resources
retreived              retrieved
retreive               retrieve
retults                results
Rewritebles            Rewritables
richt                  right
rigths                 rights
Rigt                   Right
saftey                 safety
satified               satisfied
savely                 safely
savety                 safety
scalled                scaled
scather                scatter
scenerio               scenario
sceptical              skeptical
schduler               scheduler
Sectionning            Sectioning
selction               selection
selectde               selected
sensistve              sensitive
separed                separated
separeted              separated
sepcified              specified
seperated              separated
seperately             separately
seperate               separate
seperate               separate

#INCORRECT SPELLING    CORRECTION

Seperate               Separate
seperation             separation
seperatly              separately
seperator              separator
sequencially           sequentially
sertificate            certificate
serveral               several
setted                 set
sheduled               scheduled
sheme                  scheme
shorctuts              shortcuts
shoud                  should
shouldnt               shouldn't
Shouldnt               Shouldn't
shure                  sure
Similarily             Similarly
Similiarly             Similarly
similiar               similar
simlar                 similar
simpliest              simplest
simultaneuosly         simultaneously
skript                 script
slewin                 slewing
smaple                 sample
Sombody                Somebody
somehwat               somewhat
soure                  source
sparcely               sparsely
speakiing              speaking
specefied              specified
specfic                specific
specfied               specified
specialised            specialized
specifc                specific
specifed               specified
Specificiation         Specification
specifieing            specifying
specifing              specifying
specifiy               specify
Specifiy               Specify
speficied              specified
speling                spelling
spezifying             specifying
sprectrum              spectrum
standar                standard
Startp                 Startup
Statfeul               Stateful
statfull               stateful
storeys                storys
straighforward         straightforward
streched               stretched
Streches               Stretches
Strech                 Stretch

#INCORRECT SPELLING    CORRECTION

Striked                Stroked
stuctures              structures
styleshets             stylesheets
subcribed              subscribed
subdirectorys          subdirectories
subseqently            subsequently
Substracting           Subtracting
subystem               subsystem
succeded               succeeded
succesfully            successfully
succesful              successful
succesive              successive
succesor               successor
successfull            successful
sucessfull             successful
sucessfully            successfully
sucessfuly             successfully
sucess                 success
sufficent              sufficient
superflous             superfluous
supossed               supposed
supressed              suppressed
supress                suppress
suprised               surprised
susbstitute            substitute
swaped                 swapped
synchonization         synchronization
synchronisation        synchronization
Synchronisation        Synchronization
synchronised           synchronized
synchronises           synchronizes
synchronise            synchronize
synchronyze            synchronize
Syncronization         Synchronization
syncronized            synchronized
Syncronizes            Synchronizes
syncronize             synchronize
syncronizing           synchronizing
Syncronizing           Synchronizing
syncronous             synchronous
syncrounous            synchronous
syndrom                syndrome
syntex                 syntax
synthetizer            synthesizer
syntheziser            synthesizer
sytem                  system
talbs                  tables
talse                  false
tecnology              technology
temparary              temporary
Tempertures            Temperatures
terminatin             terminating

#INCORRECT SPELLING    CORRECTION

texured                textured
themc                  them
thet                   that
threshholds            thresholds
threshhold             threshold
throtte                throttle
throught               through
throuth                through
tiggered               triggered
tihs                   this
timditiy               timidity
Timdity                Timidity
timming                timing
tranceiver             transceiver
Tranfers               Transfers
tranfer                transfer
Tranlate               Translate
tranlation             translation
transalted             translated
transation             transaction
transfering            transferring
transferrable          transferable
transmiter             transmitter
transmiting            transmitting
transmition            transmission
transmittion           transmission
transparancy           transparency
transparant            transparent
trasfered              transferred
traveller              traveler
travelling             traveling
triggerg               triggering
triggerred             triggered
truely                 truly
trys                   tries
uglyness               ugliness
unabiguous             unambiguous
unaccesible            unaccessible
unallowed              disallowed
unamed                 unnamed
unathorized            unauthorized
uncrypted              unencrypted
Uncutt                 Uncut
underlieing            underlying
underrrun              underrun
undesireable           undesirable
undestood              understood
Undexpected            Unexpected
undoedne               undid
unecessary             unnecessary
unexperienced          inexperienced
unexperience           inexperience
unfortunatly           unfortunately

#INCORRECT SPELLING    CORRECTION

Unfortunatly           Unfortunately
uniq                   unique
unitialized            uninitialized
unkown                 unknown
Unmoveable             Unmovable
unneccessary           unnecessary
unneccessay            unnecessary
unsellectected         unselected
unsuccesful            unsuccessful
unuseable              unusable
unusuable              unusable
unvailable             unavailable
uploades               uploads
upppercase             uppercase
usally                 usually
usefull                useful
usere                  user
usuable                usable
usuallly               usually
Usualy                 Usually
utilisation            utilization
vaild                  valid
valied                 valid
valueable              valuable
varb                   verb
vays                   ways
verfication            verification
verically              vertically
versins                versions
verticaly              vertically
verticies              vertices
Veryify                Verify
vicitim                victim
visualisations         visualizations
visualisation          visualization
Visualisation          Visualization
visualise              visualize
visul                  visual
volonteer              volunteer
Volumen                Volume
Voribis                Vorbis
vrtual                 virtual
waranty                warranty
watseful               wasteful
weigth                 weight
wheter                 whether
whicn                  which
whishes                wishes
whitch                 which
whith                  with

#INCORRECT SPELLING    CORRECTION

Wiazrd                 Wizard
wich                   which
wich                   which
wierd                  weird
wieving                viewing
wiev                   view
wih                    with
willl                  will
wnat                   want
workimg                working
workstatio             workstation
woud                   would
wouldd                 would
writting               writing
Writting               Writing
yeld                   yield
yorself                yourself
you'ld                 you would
yourContryCode         yourCountryCode

