#!/usr/bin/perl -w

# Check for common spelling errors.
# Artistic License, Dirk Mueller <mueller@kde.org> 2003.

# The script replaces common misspelled words with the correct words
#
# Use:
# - without parameters to check the current directory
# - with parameters to check multiple files or directories
#
# Note:
# The changes are automatic and do not follow any programming language syntax
# So be careful and check the modification before committing.

use POSIX;
use strict;

my %fix = (
	 "aasumes" => "assumes",
	 "abailable" => "available",
	 "abbriviate" => "abbreviate",
	 "abilties" => "abilities",
	 "abreviate" => "abbreviate",
	 "abbriviation" => "abbreviation",
	 "abbrevations" => "abbreviations",
	 "Abbrevation" => "Abbreviation",
	 "Ablolute" => "Absolute",
	 "acouting" => "accounting",
	 "acces" => "access",
	 "Acess" => "Access",
	 "accesible" => "accessible",
	 "accesing" => "accessing",
	 "accomodate" => "accommodate",
	 "acommodate" => "accommodate",
	 "achived" => "achieved",
	 "achive" => "achieve",
	 "achiving" => "achieving",
	 "Acknowlege" => "Acknowledge",
	 "acknoledged" => "acknowledged",
	 "acknowledgement" => "acknowledgment",
	 "Acknowledgements" => "Acknowledgments",
	 "Additinoally" => "Additionally",
	 "aconyms" => "acronyms",
	 "acording" => "according",
	 "acount" => "account",
	 "activ" => "active",
	 "addded" => "added",
	 "Additionaly" => "Additionally",
	 "additionaly" => "additionally",
	 "adressed" => "addressed",
	 "aditional" => "additional",
	 "Adminstrator" => "Administrator",
	 "adminstrator" => "administrator",
	 "anfer" => "after",
	 "aquire" => "acquire",
	 "aquired" => "acquired",
	 "accross" => "across",
	 "actons" => "actions",
	 "acually" => "actually",
	 "adapater" => "adapter",
	 "adatper" => "adapter",
	 "additionnal" => "additional",
	 "Addtional" => "Additional",
	 "additonal" => "additional",
	 "adress" => "address",
	 "adresses" => "addresses",
	 "adddress" => "address",
	 "Adress" => "Address",
	 "advertize" => "advertise",
	 "aesthetic" => "esthetic",
	 "Afganistan" => "Afghanistan",
	 "Agressive" => "Aggressive",
	 "agressive" => "aggressive",
	 "agressively" => "aggressively",
	 "alligned" => "aligned",
	 "alignement" => "alignment",
	 "allready" => "already",
	 "alrady" => "already",
	 "Allways" => "Always",
	 "alook" => "a look",
	 "alot" => "a lot",
	 "alows" => "allows",
	 "Allignment" => "Alignment",
	 "allways" => "always",
	 "alreay" => "already",
	 "alternativly" => "alternatively",
	 "ammount" => "amount",
	 "Ammount" => "Amount",
	 "analogue" => "analog",
	 "analagous" => "analogous",
	 "analizer" => "analyzer",
	 "analyse" => "analyze",
	 "analyses" => "analyzes",
	 "angainst" => "against",
	 "annoucement" => "announcement",
	 "announcments" => "announcements",
	 "anwer" => "answer",
	 "anwser" => "answer",
	 "anwsers" => "answers",
	 "appers" => "appears",
	 "appeares" => "appears",
	 "appearence" => "appearance",
	 "appeareance" => "appearance",
	 "apperarance" => "appearance",
	 "Applicalble" => "Applicable",
	 "applicaiton" => "application",
	 "aplication" => "application",
	 "appplication" => "application",
	 "appliction" => "application",
	 "appropiate" => "appropriate",
	 "approriate" => "appropriate",
	 "apropriate" => "appropriate",
	 "approximatly" => "approximately",
	 "aribrary" => "arbitrary",
	 "arbitary" => "arbitrary",
	 "arbitarily" => "arbitrarily",
	 "aribtrarily" => "arbitrarily",
	 "Arbitary" => "Arbitrary",
	 "aribtrary" => "arbitrary",
	 "arround" => "around",
	 "asssembler" => "assembler",
	 "assosciated" => "associated",
	 "assosiated" => "associated",
	 "assoziated" => "associated",
	 "asume" => "assume",
	 "assumend" => "assumed",
	 "asyncronous" => "asynchronous",
	 "asynchonous" => "asynchronous",
	 "attatchment" => "attachment",
	 "atleast" => "at least",
	 "aticles" => "articles",
	 "atomicly" => "atomically",
	 "Auxilary" => "Auxiliary",
	 "automaticaly" => "automatically",
	 "Automaticaly" => "Automatically",
	 "automaticly" => "automatically",
	 "autoreplacment" => "autoreplacement",
	 "auhor" => "author",
	 "authoratative" => "authoritative",
	 "authorisations" => "authorizations",
	 "Availible" => "Available",
	 "availble" => "available",
	 "avaible" => "available",
	 "Avaible" => "Available",
	 "availible" => "available",
	 "availibility" => "availability",
	 "avaliable" => "available",
	 "avaluate" => "evaluate",
	 "aviable" => "available",
	 "avare" => "aware",
	 "approciated" => "appreciated",
	 "auxilary" => "auxiliary",
	 "backrefences" => "backreferences",
	 "baloon" => "balloon",
	 "Basicly" => "Basically",
	 "basicly" => "basically",
	 "becuase" => "because",
	 "beautifull" => "beautiful",
	 "befor" => "before",
	 "bofore" => "before",
	 "beggining" => "beginning",
	 "begining" => "beginning",
	 "Behaviour" => "Behavior",
	 "behaviour" => "behavior",
	 "beeep" => "beep",
	 "beeing" => "being",
	 "beexported" => "be exported",
	 "Belarussian" => "Belarusian",
	 "beteen" => "between",
	 "betweeen" => "between",
	 "betrween" => "between",
	 "Blueish" => "Bluish",
	 "bruning" => "burning",
	 "botton" => "bottom",
	 "boundries" => "boundaries",
	 "boundry" => "boundary",
	 "boudaries" => "boundaries",
	 "boxs" => "boxes",
	 "buton" => "button",
	 "Buxfixes" => "Bugfixes",
	 "cancelation" => "cancellation",
	 "cancelled" => "canceled",
	 "cancelling" => "canceling",
	 "capabilites" => "capabilities",
	 "cacheing" => "caching",
	 "calulation" => "calculation",
	 "catalogue" => "catalog",
	 "cataloge" => "catalog",
	 "Cataloge" => "Catalog",
	 "catched" => "caught",
	 "ceneration" => "generation",
	 "changable" => "changeable",
	 "charater" => "character",
	 "chatwindow" => "chat window",
	 "caracters" => "characters",
	 "centre" => "center",
	 "Centre" => "Center",
	 "centralised" => "centralized",
	 "sertificate" => "certificate",
	 "chaning" => "changing",
	 "Characteres" => "Characters",
	 "charakters" => "characters",
	 "characers" => "characters",
	 "Chatacter" => "Character",
	 "charachters" => "characters",
	 "choosed" => "chose",
	 "choosen" => "chosen",
	 "Choosen" => "Chosen",
	 "cricles" => "circles",
	 "childs" => "children",
	 "chosing" => "choosing",
	 "cirumstances" => "circumstances",
	 "colaboration" => "collaboration",
	 "coloum" => "column",
	 "colour" => "color",
	 "colours" => "colors",
	 "coloumn" => "column",
	 "colum" => "column",
	 "collumns" => "columns",
	 "containg" => "containing",
	 "correponds" => "corresponds",
	 "classess" => "classes",
	 "cloumn" => "column",
	 "Coffie" => "Coffee",
	 "collecion" => "collection",
	 "commmand" => "command",
	 "comination" => "combination",
	 "Commiting" => "Committing",
	 "commiting" => "committing",
	 "comming" => "coming",
	 "comamnd" => "command",
	 "commense" => "commence",
	 "commited" => "committed",
	 "commuication" => "communication",
	 "communcation" => "communication",
	 "comparision" => "comparison",
	 "comparisions" => "comparisons",
	 "Comparision" => "Comparison",
	 "compability" => "compatibility",
	 "Compatability" => "Compatibility",
	 "compatibilty" => "compatibility",
	 "compatiblity" => "compatibility",
	 "Compedium" => "Compendium",
	 "compiiled" => "compiled",
	 "completly" => "completely",
	 "compleion" => "completion",
	 "complient" => "compliant",
	 "comsumer" => "consumer",
	 "concatonated" => "concatenated",
	 "concurent" => "concurrent",
	 "configration" => "configuration",
	 "Configuraton" => "Configuration",
	 "consequtive" => "consecutive",
	 "consecutivly" => "consecutively",
	 "continouos" => "continuous",
	 "conver" => "convert",
	 "convient" => "convenient",
	 "konstants" => "constants",
	 "contactlist" => "contact list",
	 "contexual" => "contextual",
	 "conviniently" => "conveniently",
	 "commerical" => "commercial",
	 "connent" => "connect",
	 "connnection" => "connection",
	 "constuctors" => "constructors",
	 "contigious" => "contiguous",
	 "contingous" => "contiguous",
	 "Continous" => "Continuous",
	 "continous" => "continuous",
	 "contiribute" => "contribute",
	 "controll" => "control",
	 "Contorll" => "Control",
	 "contoller" => "controller",
	 "controler" => "controller",
	 "controling" => "controlling",
	 "convinience" => "convenience",
	 "coordiator" => "coordinator",
	 "Copys" => "Copies",
	 "Coverted" => "Converted",
	 "coresponding" => "corresponding",
	 "corrent" => "correct",
	 "Costraints" => "Constraints",
	 "coypright" => "copyright",
	 "coursor" => "cursor",
	 "Coudn't" => "Couldn't",
	 "criticisim" => "criticism",
	 "cryptograhy" => "cryptography",
	 "Culculating" => "Calculating",
	 "currenty" => "currently",
	 "curren" => "current",
	 "curteousy" => "courtesy",
	 "cutsom" => "custom",
	 "customise" => "customize",
	 "Customise" => "Customize",
	 "customised" => "customized",
	 "customisation" => "customization",
	 "Custimize" => "Customize",
	 "cutt" => "cut",
	 "Cutt" => "Cut",
	 "DCOPCient" => "DCOPClient",
	 "datas" => "data",
	 "deactive" => "deactivate",
	 "Deamon" => "Daemon",
	 "Debuging" => "Debugging",
	 "Demonsrative" => "Demonstrative",
	 "debuging" => "debugging",
	 "defaul" => "default",
	 "defered" => "deferred",
	 "defintions" => "definitions",
	 "Defininition" => "Definition",
	 "deleteing" => "deleting",
	 "Denstiy" => "Density",
	 "Desription" => "Description",
	 "dependend" => "dependent",
	 "dependeds" => "depends",
	 "depricated" => "deprecated",
	 "derfined" => "defined",
	 "desidered" => "desired",
	 "deskop" => "desktop",
	 "desription" => "description",
	 "descryption" => "description",
	 "dialag" => "dialog",
	 "dialler" => "dialer",
	 "Dialler" => "Dialer",
	 "Dialling" => "Dialing",
	 "dialogue" => "dialog",
	 "disappers" => "disappears",
	 "discription" => "description",
	 "distrubutor" => "distributor",
	 "derivs" => "derives",
	 "decriptor" => "descriptor",
	 "depencies" => "dependencies",
	 "dependig" => "depending",
	 "descide" => "decide",
	 "desciptor" => "descriptor",
	 "desination" => "destination",
	 "destiantion" => "destination",
	 "desctroyed" => "destroyed",
	 "detremines" => "determines",
	 "develloped" => "developed",
	 "developped" => "developed",
	 "developerss" => "developers",
	 "diaog" => "dialog",
	 "diaglostic" => "diagnostic",
	 "didnt" => "didn't",
	 "differenciate" => "differentiate",
	 "differenly" => "differently",
	 "Differntiates" => "Differentiates",
	 "diable" => "disable",
	 "digitised" => "digitized",
	 "dificulty" => "difficulty",
	 "Difusion" => "Diffusion",
	 "diffcult" => "difficult",
	 "dirctory" => "directory",
	 "dirctely" => "directey",
	 "direcory" => "directory",
	 "directorys" => "directories",
	 "Disbale" => "Disable",
	 "disppear" => "disappear",
	 "desiabled" => "disabled",
	 "determiend" => "determined",
	 "determins" => "determines",
	 "dialling" => "dialing",
	 "diabled" => "disabled",
	 "directoy" => "directory",
	 "disactivate" => "deactivate",
	 "discpline" => "discipline",
	 "discontigous" => "discontiguous",
	 "dissassembler" => "disassembler",
	 "distingush" => "distinguish",
	 "distribtuion" => "distribution",
	 "devide" => "divide",
	 "devided" => "divided",
	 "diplayed" => "displayed",
	 "divizor" => "divisor",
	 "docucument" => "document",
	 "documentaiton" => "documentation",
	 "documentors" => "documenters",
	 "Donot" => "Do not",
	 "donnot" => "do not",
	 "doens't" => "doesn't",
	 "doesnt" => "doesn't",
	 "dont't" => "don't",
	 "dont" => "don't",
	 "Dou" => "Do",
	 "dreamt" => "dreamed",
	 "draging" => "dragging",
	 "Droped" => "Dropped",
	 "durring" => "during",
	 "duotes" => "quotes",
	 "dynamicly" => "dynamically",
	 "efficent" => "efficient",
	 "effiency" => "efficency",
	 "efficently" => "efficiently",
	 "imaginery" => "imaginary",
	 "imperical" => "empirical",
	 "eallocate" => "deallocate",
	 "editory" => "editor",
	 "embedabble" => "embeddable",
	 "embedable" => "embeddable",
	 "embeddabble" => "embeddable",
	 "embeded" => "embedded",
	 "emcompass" => "encompass",
	 "emty" => "empty",
	 "encyption" => "encryption",
	 "enhandcements" => "enhancements",
	 "enles" => "endless",
	 "enought" => "enough",
	 "entitities" => "entities",
	 "entrys" => "entries",
	 "Entrys" => "Entries",
	 "enumarated" => "enumerated",
	 "enviroment" => "environment",
	 "environent" => "environment",
	 "environemnt" => "environment",
	 "envirnment" => "environment",
	 "envirnoment" => "environment",
	 "equlas" => "equals",
	 "equiped" => "equipped",
	 "errror" => "error",
	 "errorous" => "erroneous",
	 "Equador" => "Ecuador",
	 "Evalute" => "Evaluate",
	 "espacially" => "especially",
	 "espesially" => "especially",
	 "escriptor" => "descriptor",
	 "everytime" => "every time",
	 "eample" => "example",
	 "exmaple" => "example",
	 "exacly" => "exactly",
	 "exapmle" => "example",
	 "execess" => "excess",
	 "execeeded" => "exceeded",
	 "executeble" => "executable",
	 "execuable" => "executable",
	 "exection" => "execution",
	 "exept" => "except",
	 "excecpt" => "except",
	 "existance" => "existence",
	 "exisiting" => "existing",
	 "exlusively" => "exclusively",
	 "experienceing" => "experiencing",
	 "explicitely" => "explicitly",
	 "explicity" => "explicitly",
	 "explit" => "explicit",
	 "expresions" => "expressions",
	 "extented" => "extended",
	 "extention" => "extension",
	 "extesion" => "extension",
	 "extentions" => "extensions",
	 "Extention" => "Extension",
	 "Expresion" => "Expression",
	 "fabilous" => "fabulous",
	 "falg" => "flag",
	 "familar" => "familiar",
	 "favour" => "favor",
	 "favours" => "favors",
	 "favourable" => "favorable",
	 "favourite" => "favorite",
	 "fastes" => "fastest",
	 "featue" => "feature",
	 "feeded" => "fed",
	 "filsystem" => "filesystem",
	 "firware" => "firmware",
	 "fisrt" => "first",
	 "fixiating" => "fixating",
	 "fixiated" => "fixated",
	 "fixiate" => "fixate",
	 "flaged" => "flagged",
	 "flavours" => "flavors",
	 "focussed" => "focused",
	 "follwing" => "following",
	 "folowing" => "following",
	 "Folowing" => "Following",
	 "folllowed" => "followed",
	 "formaly" => "formally",
	 "fortunatly" => "fortunately",
	 "footnotexs" => "footnotes",
	 "foward" => "forward",
	 "fragement" => "fragment",
	 "framset" => "frameset",
	 "framesyle" => "framestyle",
	 "fucntion" => "function",
	 "fuction" => "function",
	 "fuctions" => "functions",
	 "fulfilling" => "fulfiling",
	 "fullfilled" => "fulfilled",
	 "fufill" => "fulfill",
	 "Fucntion" => "Function",
	 "funcion" => "function",
	 "funciton" => "function",
	 "functin" => "function",
	 "funtion" => "function",
	 "funtional" => "functional",
	 "funtionality" => "functionality",
	 "funtions" => "functions",
	 "furthur" => "further",
	 "gaalxies" => "galaxies",
	 "Gamee" => "Game",
	 "gernerated" => "generated",
	 "ges" => "goes",
	 "giude" => "guide",
	 "guarenteed" => "guaranteed",
	 "Ghostscipt" => "Ghostscript",
	 "goind" => "going",
	 "Gostscript" => "Ghostscript",
	 "globaly" => "globally",
	 "greyed" => "grayed",
	 "grapphis" => "graphics",
	 "guaranted" => "guaranteed",
	 "gziped" => "gzipped",
	 "guarrantee" => "guarantee",
	 "handeling" => "handling",
	 "Harware" => "Hardware",
	 "harware" => "hardware",
	 "hasnt" => "hasn't",
	 "havn't" => "haven't",
	 "heigt" => "height",
	 "heigth" => "height",
	 "hiddden" => "hidden",
	 "highlighlighted" => "highlighted",
	 "Higlighting" => "Highlighting",
	 "highligting" => "highlighting",
	 "Hierachical" => "Hierarchical",
	 "honour" => "honor",
	 "honouring" => "honoring",
	 "honours" => "honors",
	 "horziontal" => "horizontal",
	 "hypens" => "hyphens",
	 "istory" => "history",
	 "i'm" => "I'm",
	 "iconized" => "iconified",
	 "indentical" => "identical",
	 "illumnating" => "illuminating",
	 "imortant" => "important",
	 "immediatly" => "immediately",
	 "immedialely" => "immediately",
	 "implemantation" => "implementation",
	 "implemenation" => "implementation",
	 "implimention" => "implementation",
	 "implmentation" => "implementation",
	 "implenetation" => "implementation",
	 "inactiv" => "inactive",
	 "incldue" => "include",
	 "innacurate" => "inaccurate",
	 "imitatation" => "imitation",
	 "innacurately" => "inaccurately",
	 "Incomming" => "Incoming",
	 "incomming" => "incoming",
	 "incovenient" => "inconvenient",
	 "independend" => "independent",
	 "Indentification" => "Identification",
	 "indepedancy" => "independency",
	 "independant" => "independent",
	 "indetectable" => "undetectable",
	 "indice" => "index",
	 "indicdate" => "indicate",
	 "indictes" => "indicates",
	 "indeces" => "indices",
	 "infinitv" => "infinitive",
	 "Inifity" => "Infinity",
	 "infomation" => "information",
	 "informatation" => "information",
	 "informationon" => "information",
	 "informations" => "information",
	 "informaion" => "information",
	 "insde" => "inside",
	 "inital" => "initial",
	 "initalized" => "initialized",
	 "initalization" => "initialization",
	 "initilization" => "initialization",
	 "intialisation" => "initialization",
	 "intialization" => "initialization",
	 "initialisation" => "initialization",
	 "Initalize" => "Initialize",
	 "initalize" => "initialize",
	 "initialising" => "initializing",
	 "Initialyze" => "Initialize",
	 "Intial" => "Initial",
	 "Initilialyze" => "Initialize",
	 "Initilize" => "Initialize",
	 "initilize" => "initialize",
	 "intiailize" => "initialize",
	 "Intialize" => "Initialize",
	 "intialize" => "initialize",
	 "initialise" => "initialize",
	 "intializing" => "initializing",
	 "isntance" => "instance",
	 "intruction" => "instruction",
	 "inteface" => "interface",
	 "Iterface" => "Interface",
	 "interfrace" => "interface",
	 "interactivelly" => "interactively",
	 "interfer" => "interfere",
	 "interisting", "interesting",
	 "interrrupt" => "interrupt",
	 "Interupt" => "Interrupt",
	 "intrrupt" => "interrupt",
	 "interrups" => "interrupts",
	 "interrumped" => "interrupted",
	 "internationalisation" => "internationalization",
	 "intervall" => "interval",
	 "intervalls" => "intervals",
	 "introdutionary" => "introductionary",
	 "introdution" => "introduction",
	 "invarient" => "invariant",
	 "invokation" => "invocation",
	 "Ionisation" => "Ionization",
	 "irrevesible" => "irreversible",
	 "is'nt" => "isn't",
	 "issueing" => "issuing",
	 "itselfs" => "itself",
	 "journalised" => "journalized",
	 "judgement" => "judgment",
	 "keyboad" => "keyboard",
	 "knowlege" => "knowledge",
	 "Konquerer" => "Konqueror",
	 "kdelbase" => "kdebase",
	 "klicking" => "clicking",
	 "kscreensave" => "kscreensaver",
	 "labelling" => "labeling",
	 "Labelling" => "Labeling",
	 "lauching" => "launching",
	 "layed" => "laid",
	 "learnt" => "learned",
	 "leats" => "least",
	 "Lenght" => "Length",
	 "lenght" => "length",
	 "Licens" => "License",
	 "licence" => "license",
	 "Licence" => "License",
	 "Licenced" => "Licensed",
	 "liset" => "list",
	 "listveiw" => "listview",
	 "listenening" => "listening",
	 "litle" => "little",
	 "localisation" => "localization",
	 "litteral" => "literal",
	 "losely" => "loosely",
	 "maintainence" => "maintenance",
	 "Malicous" => "Malicious",
	 "Maxium" => "Maximum",
	 "Minium" => "Minimum",
	 "maanged" => "managed",
	 "maching" => "matching",
	 "magnifcation" => "magnification",
	 "magnication" => "magnification",
	 "mailboxs" => "mailboxes",
	 "maillinglists" => "mailinglists",
	 "maintainance" => "maintenance",
	 "Mantainer" => "Maintainer",
	 "mamage" => "manage",
	 "managment" => "management",
	 "Managment" => "Management",
	 "manangement" => "management",
	 "mannually" => "manually",
	 "manupulation" => "manipulation",
	 "marbels" => "marbles",
	 "matchs" => "matches",
	 "maximimum" => "maximum",
	 "menues" => "menus",
	 "mesages" => "messages",
	 "messanging" => "messaging",
	 "messanger" => "messenger",
	 "Microsft" => "Microsoft",
	 "millimetres" => "millimeters",
	 "minimise" => "minimize",
	 "Minimun" => "Minimum",
	 "mimimum" => "minimum",
	 "minimising" => "minimizing",
	 "miscellaneaous" => "miscellaneous",
	 "miscellanous" => "miscellaneous",
	 "miscelaneous" => "miscellaneous",
	 "miscelanous" => "miscellaneous",
	 "Miscellanous" => "Miscellaneous",
	 "mispelled" => "misspelled",
	 "mispeled" => "misspelled",
	 "mssing" => "missing",
	 "mdified" => "modified",
	 "mdification" => "modification",
	 "Modifes" => "Modifies",
	 "modifing" => "modifying",
	 "modul" => "module",
	 "mosue" => "mouse",
	 "Mozzila" => "Mozilla",
	 "mulitple" => "multiple",
	 "multipe" => "multiple",
	 "multible" => "multiple",
	 "mutiple" => "multiple",
	 "multy" => "multi",
	 "Mulitimedia" => "Multimedia",
	 "mistery" => "mystery",
	 "neccesary" => "necessary",
	 "neccessary" => "necessary",
	 "necessery" => "necessary",
	 "nessesary" => "necessary",
	 "nedd" => "need",
	 "neet" => "need",
	 "nessecarry" => "necessary",
	 "nessecary" => "necessary",
	 "negativ" => "negative",
	 "negociated" => "negotiated",
	 "negociation" => "negotiation",
	 "neogtiation" => "negotiation",
	 "neighbour" => "neighbor",
	 "Neighbour" => "Neighbor",
	 "neighbours" => "neighbors",
	 "neighbourhood" => "neighborhood",
	 "nework" => "network",
	 "newtork" => "network",
	 "nickanme" => "nickname",
	 "Noone" => "No-one",
	 "noone" => "nobody",
	 "nonexistant" => "nonexistent",
	 "normalisation" => "normalization",
	 "noticable" => "noticeable",
	 "nucleous" => "nucleus",
	 "obtail" => "obtain",
	 "occure" => "occur",
	 "occoured" => "occurred",
	 "occouring" => "occurring",
	 "occured" => "occurred",
	 "occurance" => "occurrence",
	 "occurrance" => "occurrence",
	 "occurrances" => "occurrences",
	 "occurence" => "occurrence",
	 "occurences" => "occurrences",
	 "occurances" => "occurrences",
	 "occuring" => "occurring",
	 "ocupied" => "occupied",
	 "offical" => "official",
	 "ommited" => "omitted",
	 "onthe" => "on the",
	 "optionnal" => "optional",
	 "optimisation" => "optimization",
	 "organise" => "organize",
	 "organised" => "organized",
	 "organiser" => "organizer",
	 "Organising" => "Organizing",
	 "organising" => "organizing",
	 "organisation" => "organization",
	 "organisations" => "organizations",
	 "Organisation" => "Organization",
	 "organisational" => "organizational",
	 "orignal" => "original",
	 "Originaly" => "Originally",
	 "orginate" => "originate",
	 "opend" => "opened",
	 "oscilating" => "oscillating",
	 "orangeish" => "orangish",
	 "ouput" => "output",
	 "otehr" => "other",
	 "outputing" => "outputting",
	 "overidden" => "overridden",
	 "overriden" => "overridden",
	 "overriden" => "overridden",
	 "ownes" => "owns",
	 "pakage" => "package",
	 "panelised" => "panelized",
	 "paramter" => "parameter",
	 "paramaters" => "parameters",
	 "parametres" => "parameters",
	 "paramters" => "parameters",
	 "parametrize" => "parameterize",
	 "particip" => "participle",
	 "paticular" => "particular",
	 "particularily" => "particularly",
	 "Pendings" => "Pending",
	 "percetages" => "percentages",
	 "Perfomance" => "Performance",
	 "performace" => "performance",
	 "preformance" => "performance",
	 "Personalizsation" => "Personalization",
	 "Periferial" => "Peripheral",
	 "permissable" => "permissible",
	 "perticularly" => "particularly",
	 "permisions" => "permissions",
	 "permision" => "permission",
	 "pressentation" => "presentation",
	 "hysical" => "physical",
	 "phyiscal" => "physical",
	 "plaforms" => "platforms",
	 "plese" => "please",
	 "politness" => "politeness",
	 "posssibility" => "possibility",
	 "possebility" => "possibility",
	 "posibility" => "possibility",
	 "posibilities" => "possibilities",
	 "possebilities" => "possibilities",
	 "positon" => "position",
	 "posible" => "possible",
	 "possibilty" => "possibility",
	 "possiblity" => "possibility",
	 "potentally" => "potentially",
	 "practise" => "practice",
	 "practising" => "practicing",
	 "preceeded" => "preceded",
	 "preceeding" => "preceding",
	 "precison" => "precision",
	 "preemphasised" => "preemphasized",
	 "Preemphasised" => "Preemphasized",
	 "prefered" => "preferred",
	 "preferrable" => "preferable",
	 "Prefered" => "Preferred",
	 "presense" => "presence",
	 "prefiously" => "previously",
	 "prerequisits" => "prerequisites",
	 "priviledge" => "privilege",
	 "priviledges" => "privileges",
	 "priviliges" => "privileges",
	 "probatility" => "probability",
	 "proberly" => "properly",
	 "progess" => "progress",
	 "properies" => "properties",
	 "Propertites" => "Properties",
	 "problmes" => "problems",
	 "probelm" => "problem",
	 "proceedure" => "procedure",
	 "proctection" => "protection",
	 "programme" => "program",
	 "programm" => "program",
	 "pronunce" => "pronounce",
	 "pronunciated" => "pronounciated",
	 "pronounciation" => "pronunciation",
	 "Pronounciation" => "Pronunciation",
	 "Proxys" => "Proxies",
	 "prgramm" => "program",
	 "programing" => "programming",
	 "promille" => "per mill",
	 "Prining" => "Printing",
	 "priveleges" => "privileges",
	 "proecss" => "process",
	 "promiscous" => "promiscuous",
	 "promped" => "prompted",
	 "Propogate" => "Propagate",
	 "protoypes" => "prototypes",
	 "Psuedo" => "Pseudo",
	 "psuedo" => "pseudo",
	 "pult" => "desk",
	 "purposees" => "purposes",
	 "quatna" => "quanta",
	 "queing" => "queuing",
	 "queueing" => "queuing",
	 "Queueing" => "Queuing",
	 "querys" => "queries",
	 "quiting" => "quitting",
	 "quiten" => "quiet",
	 "readony" => "readonly",
	 "REAMDE" => "README",
	 "realise" => "realize",
	 "realy" => "really",
	 "reasonnable" => "reasonable",
	 "resonable" => "reasonable",
	 "recepient" => "recipient",
	 "recepeient" => "recipient",
	 "recevie" => "receive",
	 "recevie" => "receive",
	 "reciever" => "receiver",
	 "Recieve" => "Receive",
	 "Recieves" => "Receives",
	 "recieve" => "receive",
	 "recives" => "receives",
	 "recieves" => "receives",
	 "recieved" => "received",
	 "receieve" => "receive",
	 "receving" => "receiving",
	 "recognise" => "recognize",
	 "recognised" => "recognized",
	 "recognises" => "recognizes",
	 "recomended" => "recommended",
	 "recommanded" => "recommended",
	 "recommand" => "recommend",
	 "recommented" => "recommended",
	 "redialling" => "redialing",
	 "refered" => "referred",
	 "Refeshes" => "Refreshes",
	 "Refering" => "Referring",
	 "refreshs" => "refreshes",
	 "regarless" => "regardless",
	 "Regsiter" => "Register",
	 "regulare" => "regular",
	 "regularily" => "regularly",
	 "Reigster" => "Register",
	 "registred" => "registered",
	 "registaration" => "registration",
	 "reimplemenations" => "reimplementations",
	 "Reimplemenations" => "Reimplementations",
	 "releated" => "related",
	 "relevent" => "relevant",
	 "relocateable" => "relocatable",
	 "remaing" => "remaining",
	 "remeber" => "remember",
	 "renderes" => "renders",
	 "remebers" => "remembers",
	 "renewd" => "renewed",
	 "replys" => "replies",
	 "requeusts" => "requests",
	 "relection" => "reselection",
	 "remotley" => "remotely",
	 "reets" => "resets",
	 "resently" => "recently",
	 "reorienting" => "reorientating",
	 "Repalcement" => "Replacement",
	 "resetted" => "reset",
	 "resistent" => "resistant",
	 "ressources" => "resources",
	 "resoure" => "resource",
	 "reponsibility" => "responsibility",
	 "responsability" => "responsibility",
	 "responsivness" => "responsiveness",
	 "resposible" => "responsible",
	 "resognized" => "recognized",
	 "retreive" => "retrieve",
	 "retreived" => "retrieved",
	 "retults" => "results",
	 "Rewritebles" => "Rewritables",
	 "Rigt" => "Right",
	 "rigths" => "rights",
	 "richt" => "right",
	 "savely" => "safely",
	 "saftey" => "safety",
	 "satified" => "satisfied",
	 "savety" => "safety",
	 "scalled" => "scaled",
	 "schduler" => "scheduler",
	 "selectde" => "selected",
	 "selction" => "selection",
	 "separed" => "separated",
	 "seperate" => "separate",
	 "seperately" => "separately",
	 "seperator" => "separator",
	 "serveral" => "several",
	 "smaple" => "sample",
	 "scather" => "scatter",
	 "scenerio" => "scenario",
	 "sceptical" => "skeptical",
	 "Sectionning" => "Sectioning",
	 "Seperate" => "Separate",
	 "sequencially" => "sequentially",
	 "sheduled" => "scheduled",
	 "sheme" => "scheme",
	 "shoud" => "should",
	 "Shouldnt" => "Shouldn't",
	 "shouldnt" => "shouldn't",
	 "shorctuts" => "shortcuts",
	 "shure" => "sure",
	 "similiar" => "similar",
	 "simlar" => "similar",
	 "Similarily" => "Similarly",
	 "Similiarly" => "Similarly",
	 "simpliest" => "simplest",
	 "simultaneuosly" => "simultaneously",
	 "Sombody" => "Somebody",
	 "sensistve" => "sensitive",
	 "sepcified" => "specified",
	 "seperate" => "separate",
	 "seperatly" => "separately",
	 "seperated" => "separated",
	 "separeted" => "separated",
	 "seperation" => "separation",
	 "setted" => "set",
	 "skript" => "script",
	 "slewin" => "slewing",
	 "somehwat" => "somewhat",
	 "soure" => "source",
	 "sparcely" => "sparsely",
	 "speakiing" => "speaking",
	 "specialised" => "specialized",
	 "specfic" => "specific",
	 "specifc" => "specific",
	 "Specificiation" => "Specification",
	 "specifed" => "specified",
	 "speficied" => "specified",
	 "specefied" => "specified",
	 "specfied" => "specified",
	 "specifiy" => "specify",
	 "Specifiy" => "Specify",
	 "specifing" => "specifying",
	 "specifieing" => "specifying",
	 "speling" => "spelling",
	 "spezifying" => "specifying",
	 "sprectrum" => "spectrum",
	 "standar" => "standard",
	 "Startp" => "Startup",
	 "statfull" => "stateful",
	 "Statfeul" => "Stateful",
	 "straighforward" => "straightforward",
	 "streched" => "stretched",
	 "Strech" => "Stretch",
	 "Streches" => "Stretches",
	 "Striked" => "Stroked",
	 "storeys" => "storys",
	 "stuctures" => "structures",
	 "styleshets" => "stylesheets",
	 "subcribed" => "subscribed",
	 "subseqently" => "subsequently",
	 "subdirectorys" => "subdirectories",
	 "subystem" => "subsystem",
	 "Substracting" => "Subtracting",
	 "susbstitute" => "substitute",
	 "succeded" => "succeeded",
	 "sucess" => "success",
	 "succesful" => "successful",
	 "sucessfull" => "successful",
	 "successfull" => "successful",
	 "sucessfully" => "successfully",
	 "succesfully" => "successfully",
	 "sucessfuly" => "successfully",
	 "successfully" => "successfully",
	 "succesor" => "successor",
	 "succesive" => "successive",
	 "sufficent" => "sufficient",
	 "superflous" => "superfluous",
	 "supossed" => "supposed",
	 "supress" => "suppress",
	 "supressed" => "suppressed",
	 "suprised" => "surprised",
	 "swaped" => "swapped",
	 "synchronyze" => "synchronize",
	 "synchronise" => "synchronize",
	 "synchronises" => "synchronizes",
	 "synchronised" => "synchronized",
	 "synchronisation" => "synchronization",
	 "Synchronisation" => "Synchronization",
	 "synchonization" => "synchronization",
	 "syncrounous" => "synchronous",
	 "syncronizing" => "synchronizing",
	 "syncronized" => "synchronized",
	 "syncronous" => "synchronous",
	 "syncronize" => "synchronize",
	 "Syncronizing" => "Synchronizing",
	 "Syncronization" => "Synchronization",
	 "Syncronizes" => "Synchronizes",
	 "syndrom" => "syndrome",
	 "syntex" => "syntax",
	 "syntheziser" => "synthesizer",
	 "synthetizer" => "synthesizer",
	 "sytem" => "system",
	 "talbs" => "tables",
	 "talse" => "false",
	 "tecnology" => "technology",
	 "terminatin" => "terminating",
	 "texured" => "textured",
	 "Tempertures" => "Temperatures",
	 "temparary" => "temporary",
	 "thet" => "that",
	 "tihs" => "this",
	 "themc" => "them",
	 "threshhold" => "threshold",
	 "threshholds" => "thresholds",
	 "throtte" => "throttle",
	 "throught" => "through",
	 "throuth" => "through",
	 "timming" => "timing",
	 "transation" => "transaction",
	 "tranceiver" => "transceiver",
	 "trasfered" => "transferred",
	 "transfering" => "transferring",
	 "transferrable" => "transferable",
	 "tranlation" => "translation",
	 "tranfer" => "transfer",
	 "Tranfers" => "Transfers",
	 "Tranlate" => "Translate",
	 "transalted" => "translated",
	 "transmition" => "transmission",
	 "transmittion" => "transmission",
	 "transmiter" => "transmitter",
	 "transmiting" => "transmitting",
	 "transparancy" => "transparency",
	 "transparant" => "transparent",
	 "traveller" => "traveler",
	 "travelling" => "traveling",
	 "tiggered" => "triggered",
	 "timditiy" => "timidity",
	 "Timdity" => "Timidity",
	 "triggerred" => "triggered",
	 "triggerg" => "triggering",
	 "truely" => "truly",
	 "trys" => "tries",
	 "uglyness" => "ugliness",
	 "unabiguous" => "unambiguous",
	 "unaccesible" => "unaccessible",
	 "unallowed" => "disallowed",
	 "unamed" => "unnamed",
	 "unathorized" => "unauthorized",
	 "uncrypted" => "unencrypted",
	 "Uncutt" => "Uncut",
	 "undoedne" => "undid",
	 "unneccessay" => "unnecessary",
	 "unecessary" => "unnecessary",
	 "unneccessary" => "unnecessary",
	 "underrrun" => "underrun",
	 "underlieing" => "underlying",
	 "undestood" => "understood",
	 "undesireable" => "undesirable",
	 "Undexpected" => "Unexpected",
	 "unexperience" => "inexperience",
	 "unexperienced" => "inexperienced",
	 "Unfortunatly" => "Unfortunately",
	 "unfortunatly" => "unfortunately",
	 "uniq" => "unique",
	 "unitialized" => "uninitialized",
	 "Unmoveable" => "Unmovable",
	 "unsellectected" => "unselected",
	 "unsuccesful" => "unsuccessful",
	 "unusuable" => "unusable",
	 "unuseable" => "unusable",
	 "unkown" => "unknown",
	 "unvailable" => "unavailable",
	 "upppercase" => "uppercase",
	 "uploades" => "uploads",
	 "usuable" => "usable",
	 "usally" => "usually",
	 "usuallly" => "usually",
	 "Usualy" => "Usually",
	 "usefull" => "useful",
	 "usere" => "user",
	 "utilisation" => "utilization",
	 "valied" => "valid",
	 "vaild" => "valid",
	 "varb" => "verb",
	 "vays" => "ways",
	 "verfication" => "verification",
	 "verically" => "vertically",
	 "verticaly" => "vertically",
	 "verticies" => "vertices",
	 "versins" => "versions",
	 "Veryify" => "Verify",
	 "vicitim" => "victim",
	 "visul" => "visual",
	 "visualise" => "visualize",
	 "visualisation" => "visualization",
	 "Visualisation" => "Visualization",
	 "visualisations" => "visualizations",
	 "Volumen" => "Volume",
	 "Voribis" => "Vorbis",
	 "vrtual" => "virtual",
	 "waranty" => "warranty",
	 "watseful" => "wasteful",
	 "weigth" => "weight",
	 "wheter" => "whether",
	 "whith" => "with",
	 "within" => "within",
	 "whitch" => "which",
	 "wich" => "which",
	 "wih" => "with",
	 "whicn" => "which",
	 "whishes" => "wishes",
	 "wierd" => "weird",
	 "wieving" => "wieving",
	 "willl" => "will",
	 "Wiazrd" => "Wizard",
	 "wnat" => "want",
	 "workimg" => "working",
	 "workstatio" => "workstation",
	 "woud" => "would",
	 "wouldd" => "would",
	 "Writting" => "Writing",
	 "writting" => "writing",
	 "yeld" => "yield",
	 "yorself" => "yourself",
	 "yourContryCode" => "yourCountryCode",
	 "you'ld" => "you would",
	 "wich" => "which"
	 );

sub spell_file($)
{
    my ($f) = @_;
    my $firsttime = 1;

    if(open(IN, "<$f")) {
        my @c = <IN>;

        my $matches = 0;
        foreach my $line (@c) {
            my @words = split /\W/, $line;
            foreach my $w (@words) {
                if(defined($fix{$w})) {
                    $matches++;
                    $line =~ s/\b$w\b/$fix{$w}/g;
                }
            }
            foreach my $w (keys %fix) {
                if ($line =~ /$w/ and $line !~ /$fix{$w}/) {
                    if ($firsttime) {
                        print "spelling $f\n";
                        $firsttime = 0;
                    }
                    print "nonword misspelling: $w\n";
                }
            }
        }
        close (IN);
        if($matches) {
            open (O, ">$f");
            print O @c;
            close(O);
        }
    }
}

my @dirqueue = ();

sub processDir($)
{
    my ($d) = @_;
    my $e;

    print "processDir: $d\n";

    opendir (DIR, "$d") || die "couldn't read dir: $d";
    while ($e = readdir(DIR)) {
        next if ($e eq ".");
        next if ($e eq "..");
        next if ($e eq "CVS");
        next if ($e =~ /\.desktop$/);
        next if ($e =~ /^\./);
        next if ($e =~ /\.moc$/);
        push (@dirqueue, "$d/$e") if (-d ("$d/$e"));
        next if (-d ("$d/$e"));

        my $type = `file $d/$e`;
        if ($type !~ /(text|ASCII)/i) {
            print "** Skipping $d/$e\n";
            next;
        }
        &spell_file("$d/$e") if (-f ("$d/$e"));
    }
    closedir(DIR);
}

if ($#ARGV >= 0) {
    while (defined($ARGV[0])) {
        $_ = shift;
        if (-d $_) {
            push (@dirqueue, $_);
        }
        elsif (-f $_) {
            print "processing file: " . $_ . "\n";
            spell_file( $_ );
        }
        else {
            print STDERR "unknown file: " . $_ . "\n";
        }
    }
}
else {
    # No files were given on the command line, so assume current directory
    push (@dirqueue, getcwd());
}

while($#dirqueue >= 0) {
    processDir( pop @dirqueue );
}

