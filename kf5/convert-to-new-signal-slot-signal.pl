#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# Convert to new signal/slot syntax
# it's totally experimental
# find -iname "*.cpp" |xargs kde-dev-scripts/kf5/convert-to-new-signal-slot-signal.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;
my $activateDebug = 1;
my %varname = ();
my $headerclassname;
my $numberOfClassName=0;
my %uiclassname = ();
my %localuiclass = ();
my %listOfClassName = ();
my %overloadedSlots = ();
my %privateSlots = ();
my %privateVariableWithPointer = ();

sub rewriteConnectFunction($$$$$$)
{
    my ($indent, $sender, $signal, $receiver, $slot, $lastArgument) = @_;
    my $myNewLine;
    if (defined $lastArgument) {
       # lastArgument has ')'
       warn "last argument :$lastArgument\n";
       $myNewLine = $indent . "connect($sender, $signal, $receiver, &$slot, $lastArgument;\n";
    } else {
       $myNewLine = $indent . "connect($sender, $signal, $receiver, &$slot);\n";
    }
}
 

# sets $_ if classname+function+arguments matches an overloaded signal (testSignal+testArguments)
sub checkOverloadedSignal($$$$$)
{
    my ($classname, $function, $arguments, $testSignal, $testArguments) = @_;
    my $initialArguments = $arguments; # hopefully includes the const-ref for a QString..
    my $initialTestArguments = $testArguments;
    $testArguments =~ s/const (.*)\s*&/$1/;
    $arguments =~ s/const (.*)\s*&/$1/;
    # Be sure to remove space. I don't understand why const (.*)\s*&/$1/; keeps space in $testArguments
    $testArguments =~ s, ,,g;
    if (($arguments eq $testArguments) and ($function eq $testSignal)) {
        $_ = "static_cast<void ($classname" . "::*)$initialTestArguments>(&" . "$classname" . "::$function)";
        print STDERR $_ . "\n";
    }
}

sub cast_overloaded_signal($$$)
{
    my ($classname, $argument, $function) = @_;
    local $_;
    warn "ClassName \'$classname\' Argument \'$argument\' Function \'$function\'\n";
    if ($classname eq "QCompleter") {
        checkOverloadedSignal($classname, $function, $argument, "activated", "(const QString &)");
        checkOverloadedSignal($classname, $function, $argument, "activated", "(const QModelIndex &)");
        checkOverloadedSignal($classname, $function, $argument, "highlighted", "(const QString &)");
        checkOverloadedSignal($classname, $function, $argument, "highlighted", "(const QModelIndex &)");

    } elsif ($classname eq "KNotification") {
        checkOverloadedSignal($classname, $function, $argument, "activated", "(unsigned int)");
        checkOverloadedSignal($classname, $function, $argument, "activated", "(uint)");
    } elsif ($classname =~ /Combo/) {
        checkOverloadedSignal($classname, $function, $argument, "returnPressed", "()");
        checkOverloadedSignal($classname, $function, $argument, "returnPressed", "(const QString&)");
        checkOverloadedSignal($classname, $function, $argument, "activated", "(int)");
        checkOverloadedSignal($classname, $function, $argument, "activated", "(const QString &)");
        checkOverloadedSignal($classname, $function, $argument, "highlighted", "(int)");
        checkOverloadedSignal($classname, $function, $argument, "highlighted", "(const QString &)");
        checkOverloadedSignal($classname, $function, $argument, "currentIndexChanged", "(int)");
        checkOverloadedSignal($classname, $function, $argument, "currentIndexChanged", "(const QString &)");
    } elsif ($classname eq "QButtonGroup") {
        checkOverloadedSignal($classname, $function, $argument, "buttonReleased", "(QAbstractButton *)");
        checkOverloadedSignal($classname, $function, $argument, "buttonReleased", "(int)");
        checkOverloadedSignal($classname, $function, $argument, "buttonClicked", "(QAbstractButton *)");
        checkOverloadedSignal($classname, $function, $argument, "buttonClicked", "(int)");
        checkOverloadedSignal($classname, $function, $argument, "buttonPressed", "(QAbstractButton *)");
        checkOverloadedSignal($classname, $function, $argument, "buttonPressed", "(int)");
        checkOverloadedSignal($classname, $function, $argument, "buttonToggled", "(QAbstractButton *)");
        checkOverloadedSignal($classname, $function, $argument, "buttonToggled", "(int)");
    } elsif (($classname =~ /SpinBox/) ) {
        checkOverloadedSignal($classname, $function, $argument, "valueChanged", "(int)");
        checkOverloadedSignal($classname, $function, $argument, "valueChanged", "(const QString &)");
    } elsif (($classname eq "KTabWidget") or ($classname =~ /Tab/)) { # has a virtual closeRequest(int) but that's not a signal
        checkOverloadedSignal($classname, $function, $argument, "closeRequest", "(QWidget *)");
    } elsif (($classname eq "KSelectAction") or ($classname eq "KFontAction")) {
        checkOverloadedSignal($classname, $function, $argument, "triggered", "(const QString &)");
        checkOverloadedSignal($classname, $function, $argument, "triggered", "(int)");
        #checkOverloadedSignal($function, $argument, "triggered", "(QAction \*)");
    } elsif ($classname eq "KUrlLabel") {
        checkOverloadedSignal($classname, $function, $argument, "leftClickedUrl", "(const QString &)");
        checkOverloadedSignal($classname, $function, $argument, "leftClickedUrl", "()");
        checkOverloadedSignal($classname, $function, $argument, "rightClickedUrl", "(const QString &)");
        checkOverloadedSignal($classname, $function, $argument, "rightClickedUrl", "()");
    } elsif ($classname eq "QTcpSocket") {
        checkOverloadedSignal($classname, $function, $argument, "error", "(QAbstractSocket::SocketError)");
        checkOverloadedSignal($classname, $function, $argument, "error", "()");
    } elsif ($classname eq "Akonadi::ChangeRecorder") {
        checkOverloadedSignal($classname, $function, $argument, "collectionChanged", "(const Akonadi::Collection &)");
        checkOverloadedSignal($classname, $function, $argument, "collectionChanged", "(const Akonadi::Collection &, const QSet<QByteArray &)");
    } elsif ($classname eq "KUrlRequester" ) {
        checkOverloadedSignal($classname, $function, $argument, "returnPressed", "(const QString &)");
        checkOverloadedSignal($classname, $function, $argument, "returnPressed", "()");
    } elsif ($classname eq "QNetworkReply" ) {
        checkOverloadedSignal($classname, $function, $argument, "error", "(QNetworkReply::NetworkError)");
    } elsif ($classname eq "QSignalMapper" ) {
        checkOverloadedSignal($classname, $function, $argument, "mapped", "(int)");
        checkOverloadedSignal($classname, $function, $argument, "mapped", "(QWidget *)");
        checkOverloadedSignal($classname, $function, $argument, "mapped", "(const QString &)");
        checkOverloadedSignal($classname, $function, $argument, "mapped", "(QObject *)");

    }

    if ( defined $_ ) {
       return $_;
    } else {
       return "&" . "$classname::$function";
    }
}

# initialize variable before parsing a new file
sub initVariables
{
    %varname = ();
    $headerclassname = "";
    $numberOfClassName = 0;
    %uiclassname = ();
    %localuiclass = ();
    %listOfClassName = ();
    %overloadedSlots = ();
    %privateSlots = ();
    %privateVariableWithPointer = ();
}

# add new variable with its type.
sub addToVarName($$)
{
    my ($classname, $var) = @_;
    if (not $classname eq ":" and not $classname eq "return") { 
      #If we found variable in header don't overwrite it
      #if (not defined $varname{$var}) {
          $varname{$var} = ${classname}; 
          if (defined $activateDebug) {
              warn "new variable added: \'$var\' className :\'$classname\'\n";
          }
      #}
   }
}

# Clean sender variable
sub cleanSender($)
{
    my ($var) = @_;
    $var =~ s/^\(//;
    $var = functionUtilkde::cleanSpace($var);
    return $var;
}

# input: class::slot
# output: notpossible string if this slot has default values
sub checkOverloadedSlot($)
{
    my ($fullslot) = @_;
    if (my ($class, $slot) = $fullslot =~ m/(.*)::([_\w]+)/) {
        return defined $overloadedSlots{$class}{$slot} ? "slot with default value(s)" : undef;
    }
    warn "Unparsable fullslot: $fullslot\n";
    return undef;
}

# input: class::slot
# output: notpossible string if this slot is a Q_PRIVATE_SLOT
sub checkPrivateSlot($)
{
    my ($fullslot) = @_;
    if (my ($class, $slot) = $fullslot =~ m/(.*)::([_\w]+)/) {
        return defined $privateSlots{$class}{$slot} ? "slot is a Q_PRIVATE_SLOT" : undef;
    }
    warn "Unparsable fullslot: $fullslot\n";
    return undef;  
}

# extract argument from signal
sub extraArgumentFunctionName($)
{
    my ($line) = @_;
    my $argument;
    warn "$line \n";
    my $regexpArgument = qr/
                    ^\(.*
                    ${functionUtilkde::paren_begin}1${functionUtilkde::paren_end}
                    \).*$                        
                    /x; # /x Enables extended whitespace mode
    if ( my ($argument2) = $line =~ $regexpArgument) {
       $argument = $argument2;
    }
    return $argument;
}

# extract function name
sub extractFunctionName($)
{
    my ($line) = @_;
    my $regexpSignal = qr/
                    ^\(\s*
                    (\w+)                      # (1) functionname
                    .*$                        
                    /x; # /x Enables extended whitespace mode
    if ( my ($functionname) = $line =~ $regexpSignal) {
       $line = $1;
    }
    return $line;
}

# Parse the current line for variable declarations
sub parseLine($)
{
    my ($file) = @_;
    if (/Ui::(\w+)\s+(\w+);/ || /Ui::(\w+)\s*\*\s*(\w+);/ || /Ui_(\w+)\s*\*\s*(\w+)/) {
        my $uiclass = $1;
        my $uivariable = $2;
        warn "$file: $uiclass :  $uivariable \n";
        if (defined $uiclassname{$uiclass}) {
            if (defined $activateDebug) {
                warn "Found ui class \'$uiclass\' uivariable \'$uivariable\' \n";
            }
            $localuiclass{$uivariable} = $uiclass;
        }
    }
    # Foo toto;
    if ( /^\s*([:_\w]+)\s+([_\w]+);/) {
        my $classname = $1;
        my $var = $2;
        if ($classname ne "delete" and $classname ne "class" and $classname ne "struct" and $classname ne "return") {
            #print STDERR "CASE 1. classname='$classname'\n";
            addToVarName($classname, $var);
        }
    }

    # Foo toto =
    if ( /^\s*([:_\w]+)\s+([_\w]+)\s*=/) {
        my $classname = $1;
        my $var = $2;
        #print STDERR "CASE 2. classname='$classname'\n";
        addToVarName($classname, $var);
    }
    # Foo *toto =
    if ( /^\s*([:_\w]+)\s*\*\s*([_\w]+)\s*=/) {
        my $classname = $1;
        my $var = $2;
        #print STDERR "CASE 6. classname='$classname'\n";
        addToVarName($classname, $var);
    }

    # Foo toto(...)
    # Unfortunately this also catches function declarations in header files, they look pretty much the same.
    # All we can do is filter out those that return void. But int a(5); and int a(int param); are not very different.
    if ( /^\s*([:_\w]+)\s+([_\w]+)\(/) {
        my $classname = $1;
        my $var = $2;
        if ($classname ne "else" and $classname ne "void") {
            #print STDERR "CASE 3. classname='$classname'\n";
            addToVarName($classname, $var);
        }
    }

    # Private* d;
    # Private* const d;
    # AttachmentControllerBase *const q;
    if (/^\s*([:_\w]+)\s*\*\s*(?:const)?\s*([:_\w]+);/ ) {
        my $classname = $1;
        my $var = $2;
        #print STDERR "CASE 4. classname='$classname'\n";
        addToVarName($classname, $var);
    }
    if (/^\s*QPointer\<\s*([:_\w]+)\s*\>\s*([:_\w]+);/ ) {
        my $classname = $1;
        my $var = $2;
        #print STDERR "CASE 5. classname='$classname'\n";
        if (not defined $privateVariableWithPointer{$var}) {
            if (defined $activateDebug) {
                warn "Found private variable with QPointer class \'$classname\' variable \'$var\' \n";
            }
            $privateVariableWithPointer{$var} = $classname;
        }
    }
}

sub parseUiFile()
{
    open(my $ALLFILE, "-|", qw(find . -type f));
    my $uifile;
    while ($uifile = <$ALLFILE>) {
      next if not $uifile =~ /\.ui/;
      chomp $uifile;
      open(my $FILE, "<", $uifile) or warn "We can't open file $uifile:$!\n";
      warn "Open file $uifile\n";
      my $mainClassFound;
      my @lui = map {
        #<widget class="QProgressBar" name="progressBar" >
        if (/\<widget class=\"(.*)\" name=\"(\w+)\"/) {
           my $className = $1;
           my $variableName = $2;
           if (defined $activateDebug) {
               warn "Found class in ui file \'$uifile\', className: \'$className\', variable: \'$variableName\'\n";
           }
           $varname{$variableName} = ${className};
        }
        if (/\<class\>(.*)\<\/class\>/) {
           if (not defined $mainClassFound) {
             my $name = $1;
             $uiclassname{$name} = 1;
             if (defined $activateDebug) {
                 warn "Found Class Name in file \'$uifile\': name \'$name\'\n";
             }
             $mainClassFound = 1;
           }
        }
        # Special case buttongroup
        if ( /\<buttongroup name=\"(.*)\"\/\>/ ) {
           my $className = "QButtonGroup";
           my $variableName = $1;
           $varname{$variableName} = ${className};

           if (defined $activateDebug) {
              warn "Found QButtonGroup in file \'$uifile\': name \'$variableName\'\n";
           }
        }

        $_;
      } <$FILE>
    }
}

sub parseHeaderFile($)
{
    my ($file) = @_;
    my $header = functionUtilkde::headerName($file);
    my $inslots = 0;
    warn "Parse header file: $header \n";
    # parse header file
    open(my $HEADERFILE, "<", $header) or warn "We can't open file $header:$!\n";
    my @lheader = map {
        my $orig = $_;
        my $regexp = qr/
           ^(\s*)                        # (1) Indentation
           ([:\w]+)                      # (2) Classname
           \*\s*                         #     *
           ([:\w]+)\s*                   # (3) variable name
           ;                             #     ;
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $classname, $var) = $_ =~ $regexp) {
           $classname = functionUtilkde::cleanSpace($classname);
            if (defined $activateDebug) {
               warn "$header file: found classname \'$classname\' variable: \'$var\'\n";
           }
           $varname{$var} = ${classname};
        }

        # Search header class name
        if (/class\s*(?:\w+EXPORT|\w+DEPRECATED)?\s*(\w+)\s*:\s*public\s*(.*)/) {
           my $class = $1;
           my $parentClass = $2;
           if (defined $activateDebug) {
              warn "FOUND Class \'$class\' parentClass: \'$parentClass\' $_\n";
           }
           $headerclassname = $class;
           $overloadedSlots{$headerclassname} = ();
           $listOfClassName{$headerclassname} = 1;
           $numberOfClassName++;
        }

        # Parse slots, to detect overloads
        if (/^\s*(?:public|protected|private|signals|Q_SIGNALS)\s*(?:slots|Q_SLOTS)?\s*:/) {
            if (/slots/i) {
                $inslots = 1;
            } else {
                $inslots = 0;
            }
        }
        if ($inslots) {
            my $function_regexp = qr/
               ^(\s*)                                                         # (1) Indentation
               ([:\w]+)\s*                                                    # (2) Return type
               ([:\w]+)\s*                                                    # (3) Function name
               ${functionUtilkde::paren_begin}4${functionUtilkde::paren_end}  # (4) (args)
               /x; # /x Enables extended whitespace mode
            if (my ($indent, $return, $function, $args) = $_ =~ $function_regexp) {
                if ($args =~ /=/) { # slot with default values
                    $overloadedSlots{$headerclassname}{$function} = 1;
                }
            }
            # TODO also detect real overloads (seenSlots -> if already there, add to overloadedSlots)
        }

        #Q_PRIVATE_SLOT( d, void attachmentRemoved( MessageCore::AttachmentPart::Ptr ) )

        my $qprivateSlot = qr/
           ^(\s*)                        # (1) Indentation
           Q_PRIVATE_SLOT\s*\(\s*
           ([:\w]+)\s*                   # (2) private variable
           ,\s*([:\w]+)\s*               # (3) Return type
           ([:\w]+)\s*                   # (4) Function name
           ${functionUtilkde::paren_begin}5${functionUtilkde::paren_end}  # (5) (args)

           (.*)$                         # (6) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $privateVariable, $return, $function, $args, $after) = $_ =~ $qprivateSlot) {
           warn "found private slot  $args $_\n";
           $privateSlots{$headerclassname}{$function} = $args;
        }

        parseLine($file);

        $_;
    } <$HEADERFILE>;
    warn "We have $numberOfClassName class in $header\n";
}

foreach my $file (@ARGV) {
    
    # 1) initialize variable before to parse file
    initVariables();
    
    # 2) Search all ui file and parse them
    parseUiFile();
    
    # 3) read header and parse it.
    parseHeaderFile($file);

    my $currentLine = 1;

    # 4) Parse cpp file
    my $modified;
    my $tojoin;
    my $toorig;
    my %varnamewithpointer = ();
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        # private class in cpp file.
        if (/Ui::(\w+)\s+(\w+);/ || /Ui::(\w+)\s*\*\s*(\w+);/ || /Ui_(\w+)\s*\*\s*(\w+)/) {
           my $uiclass = $1;
           my $uivariable = $2;
           warn "$file: $uiclass :  $uivariable \n";
           if (defined $uiclassname{$uiclass}) {
              if (defined $activateDebug) {
                 warn "Found ui class \'$uiclass\' uivariable \'$uivariable\' \n";
              }
              $localuiclass{$uivariable} = $uiclass;
           }
        }

        my $regexp = qr/
           ^(\s*)                        # (1) Indentation
           (.*?)                         # (2) Possibly "Classname *" (the ? means non-greedy)
           (\w+)                         # (3) variable name
           \s*=\s*                       #   assignment
           new\s+([:\w]+)                # (4) class name
           (.*)$                         # (5) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $var, $classname, $afterreg) = $_ =~ $regexp) {
           $classname = functionUtilkde::cleanSpace($classname); 
           $var = functionUtilkde::cleanSpace($var);
           #If we found variable in header don't overwrite it
           if (not defined $varname{$var} and not defined $privateVariableWithPointer{$var}) {
                if (defined $activateDebug) {
                   warn "$file: cpp file: found classname \'$classname\' variable: \'$var\' $_\n";
                }
                if ( $left =~ /QPointer\<$classname\>/) {
                   $varnamewithpointer{$var} = ${classname};
                } else {
                   $varname{$var} = ${classname};
                }
           }
        }
        if (/(\w+)\s*\*\s*(\w+)\s*=.*addAction\s*\(/) {
           my $classname = $1;
           my $var = $2;
           addToVarName($classname, $var);
        }

        #QPushButton *okButton = buttonBox->button(QDialogButtonBox::Ok);
        if (/(\w+)\s*\*\s*(\w+)\s*=.*buttonBox\-\>button\s*\(QDialogButtonBox\b/) {
           my $classname = $1;
           my $var = $2;
           addToVarName($classname, $var);
        }

        parseLine($file);


        if ( /^\s*([:\w]+)::([~\w]+).*/ ) {
	   my $currentClass = $1;
	   my $currentFunctionName = $2;
	   #warn "We are in a constructor: currentClass: \'$currentClass\', function name \'$currentFunctionName\'\n";
	   if (defined $listOfClassName{$currentClass}) {
               #warn "it's an header class\n";
	       $headerclassname = $currentClass;
	   }
           
        } elsif ( /^([:\w]+)\s*\*\s*(\w+)::([~\w]+)\.*/ || /^([:\w]+)\s*(\w+)::([~\w]+)\.*/) {
	   my $currentClass = $2;
	   my $currentFunctionName = $3;
	   my $currentReturnFunction = $1;
	   #warn "We are in a function : currentClass: \'$currentClass\', function name \'$currentFunctionName\', return type \'$currentReturnFunction\'\n";
	   if (defined $listOfClassName{$currentClass}) {
               #warn "it's an header class\n";
	       $headerclassname = $currentClass;
	   }	   
	}
        
        # Verify comment
        if ( defined $tojoin) {
           
           $toorig .= $_;
           
           $tojoin =~ s/\s*\n$//; # remove any trailing space
           $_ =~ s/^\s*/ /; # replace indent with single space
           $_ = $tojoin . $_;
           warn "look at end ? \'$_\'\n";
           if ( /;\s*$/ || /;\s*\/\*\.*\*\// ) {
             undef $tojoin;          
           } 
        }
        
        my $regexpConnect = qr/
          ^(\s*(?:[\-\>:\w]+)?)           # (1) Indentation, optional classname or variable name
          connect\s*
          ${functionUtilkde::paren_begin}2${functionUtilkde::paren_end}  # (2) (args)         
          ;/x; # /x Enables extended whitespace mode
        if (my ($indent, $argument) = $_ =~ $regexpConnect ) {
           if (defined $activateDebug) {
              warn "ARGUMENT $argument\n";
           }
           my ($sender, $signal, $receiver, $signalorslot, $slot, $lastArgument, $after);
           my $connectArgument_regexp = qr/
                                 ^([^,]*)\s*                 # (1) sender
                                 ,\s*SIGNAL\s*${functionUtilkde::paren_begin}2${functionUtilkde::paren_end}\s*     # (2) signal
                                 ,\s*([^,]*)                 # (3) receiver
                                 ,\s*(\w+)\s*                # (4) SLOT or SIGNAL
                                 ${functionUtilkde::paren_begin}5${functionUtilkde::paren_end}          # (5) slot
                                 (?:,\s([^,]*))?             # (6) Last argument in slot as Qt::QueuedConnection
                                 (.*)$                       # (7) after
                                 /x;
           if ( ($sender, $signal, $receiver, $signalorslot, $slot, $lastArgument, $after) = $argument =~ $connectArgument_regexp) {
           
              # We can have SIGNAL/SIGNAL or SIGNAL/SLOT
              if (($signalorslot eq "SIGNAL") or ($signalorslot eq "SLOT")) {
                warn "11Without arguments: SENDER: \'$sender\'  SIGNAL: \'$signal\' RECEIVER: \'$receiver\' SLOT: \'$slot\' \n";
                $sender = cleanSender($sender);
                my $signalArgument = extraArgumentFunctionName($signal);
                $signal = extractFunctionName($signal);
                $slot = extractFunctionName($slot);
                my $localSenderVariable;
                my $localReceiverVariable;
                if ( $sender =~ /^&/) {
                   $sender =~ s/^&//;
                   $localSenderVariable = 1;
                }
                if ( $receiver =~ /^&/) {
                   $receiver =~ s/^&//;
                   $localReceiverVariable = 1;
                }

                my $notpossible;
                if ( (defined $varname{$sender}) and (defined $varname{$receiver}) ) {
                    $signal = cast_overloaded_signal($varname{$sender}, $signalArgument, $signal);
                    $slot = "$varname{$receiver}::$slot";
                    if ( defined $localSenderVariable) {
                       $sender = "&" . $sender;
                    }
                    if ( defined $localReceiverVariable) {
                       $receiver = "&" . $receiver;
                    }
                    if (not defined $notpossible) {
                        $notpossible = checkOverloadedSlot($slot);
                    }
                    if (not defined $notpossible) {
                        $notpossible = checkPrivateSlot($slot);
                    }
                    if (not defined $notpossible) {
                        $_ = rewriteConnectFunction($indent, $sender, $signal, $receiver, $slot, $lastArgument);
                        undef $toorig;
                    }
                } else {
                  my $classWithQPointer;
                  my $receiverWithQPointer;
                  if ( defined $varname{$sender} ) {
                    $signal = cast_overloaded_signal($varname{$sender}, $signalArgument, $signal);
                  } elsif ( defined $privateVariableWithPointer{$sender} ) {
                    $signal = cast_overloaded_signal($privateVariableWithPointer{$sender}, $signalArgument, $signal);
                    $classWithQPointer = 1;                    
                  } elsif ( defined $varnamewithpointer{$sender} ) {
                    $signal = cast_overloaded_signal($varnamewithpointer{$sender}, $signalArgument, $signal);
                    $classWithQPointer = 1;
                  } elsif ( $sender eq "this") {
                    if ( $headerclassname eq "" ) {
                       $notpossible = "sender is 'this' but I don't know the current classname";
                    }
                    $signal = cast_overloaded_signal($headerclassname, $signalArgument, $signal);
                  } elsif ( $sender eq "qApp") {
                     $signal = "&QApplication::$signal";
                  } elsif ( $sender eq "kapp") {
                     $signal = "&KApplication::$signal";
                  } elsif ( $sender =~ /button\(QDialogButtonBox::/) {
                    $signal = "&QPushButton::$signal";
                  } elsif ( $sender =~ /actionCollection\(\)\-\>action\b/) {
                    $signal = "&QAction::$signal";
                  } elsif ( $sender =~ /(.*)::self\(\)/) {
                    my $class = $1;
                    $signal = "&" . "$class::$signal";
                  } else {
                    # It's not specific to ui class. It can be a private class too
                    if ( $sender =~ /(\w+)\.(.*)/  || $sender =~ /(\w+)\-\>(.*)/) {
                       my $uivariable = $1;
                       my $varui = $2;
                       if (defined $activateDebug) {
                          warn "22UI VARIABLE :$uivariable\n";
                       }
                       if (defined $localuiclass{$uivariable} ) {
                           if (defined $activateDebug) {
                              warn "variable defined  $varui\n";
                           }
                           if ( defined $varname{$varui} ) {
                              if (defined $activateDebug) {
                                 warn "vartype found $varname{$varui} \n";
                              }
                              $signal = cast_overloaded_signal($varname{$varui}, $signalArgument, $signal);
                           } else {
                             $notpossible = "unknown variable $varui";
                           }
                       } elsif (defined $varname{$uivariable}) {
                           if ( defined $varname{$varui} ) {
                              $signal = cast_overloaded_signal($varname{$varui}, $signalArgument, $signal);

                              if (defined $activateDebug) {
                                 warn "vartype found $varname{$varui} \n";
                              }
                           } else {
                             $notpossible = "unknown variable $varui";
                           }
                       } else {
                         $notpossible = "unknown variable $uivariable";
                       }
                    } else {
                       $notpossible = "unparsed sender $sender";
                    }
                  }
                  if (not defined $notpossible) {

                    if ( defined $varname{$receiver} ) {
                      $slot = "$varname{$receiver}::$slot";
                    } elsif ( defined $varnamewithpointer{$receiver} ) {
                      $slot = "$varnamewithpointer{$receiver}::$slot";
                      $receiverWithQPointer = 1;
                    } elsif ( defined $privateVariableWithPointer{$receiver} ) {
                      $slot = "$privateVariableWithPointer{$receiver}::$slot";
                      $receiverWithQPointer = 1;
                    } elsif ( $receiver eq "this") {
                      if ( $headerclassname eq "" ) {
                         $notpossible = "no current classname";
                      }
                      $slot = "$headerclassname::$slot";
                    } else {
                       if ( $receiver =~ /(\w+)\.(.*)/  || $receiver =~ /(\w+)\-\>(.*)/) {
                          my $uivariable = $1;
                          my $varui = $2;
                          if (defined $activateDebug) {
                             warn "11UI VARIABLE :$uivariable\n";
                          }
                          if (defined $localuiclass{$uivariable} ) {
                              if (defined $activateDebug) {
                                  warn "variable defined  $varui\n";
                              }
                              if ( defined $varname{$varui} ) {
                                  if (defined $activateDebug) {
                                      warn "vartype found $varname{$varui} \n";
                                 }
                                 $slot = "$varname{$varui}::$slot";
                              } else {
                                 $notpossible = "unknown variable $varui";
                              }
                          } else {
                             $notpossible = "no class for $uivariable";
                          }
                       } else {
                           $notpossible = "receiver $receiver is unknown";
                       }
                    }
                  }
                  if (not defined $notpossible) {
                      $notpossible = checkOverloadedSlot($slot);
                  }
                  if (not defined $notpossible) {
                      $notpossible = checkPrivateSlot($slot);
                  }

                  if (not defined $notpossible) {
                     if ( defined $localSenderVariable) {
                         $sender = "&" . $sender;
                     }
                     if ( defined $localReceiverVariable) {
                         $receiver = "&" . $receiver;
                     }
                     if ( defined $classWithQPointer) {
                         $sender .= ".data()";
                     }
                     if (defined $receiverWithQPointer) {
                         $receiver .= ".data()";
                     }
                     $_ = rewriteConnectFunction($indent, $sender, $signal, $receiver, $slot, $lastArgument);
                     undef $toorig;
                  } else {
                       my $line = $_;
                       chomp $line;
                       warn "$file : line $currentLine : Can not convert \'$line\' because $notpossible\n";
                       if (defined $toorig) {
                           $_ = $toorig;
                           undef $toorig;
                       }
                  }
                }
                if (defined $activateDebug) {
                    warn "AFTER Without arguments: SENDER: \'$sender\'  SIGNAL: \'$signal\' RECEIVER: \'$receiver\' SLOT: \'$slot\' \n";
                }
              }
           } else {
                my $connectArgument2_regexp = qr/
                                 ^([^,]*)\s*                                                       # (1) sender
                                 \s*,\s*SIGNAL\s*
                                 ${functionUtilkde::paren_begin}2${functionUtilkde::paren_end}     # (2) signal
                                 ,\s*(\w+)\s*                                                      # (3) SLOT or SIGNAL
                                 ${functionUtilkde::paren_begin}4${functionUtilkde::paren_end}     # (4) slot
                                 (?:,\s([^,]*))?                                                   # (5) Last argument in slot as Qt::QueuedConnection
                                 (.*)$                                                             # (6) after
                                 /x;
                if ( ($sender, $signal, $signalorslot, $slot, $lastArgument, $after) = $argument =~ $connectArgument2_regexp) {
                   # We can have SIGNAL/SIGNAL or SIGNAL/SLOT
                   if (($signalorslot eq "SIGNAL") or ($signalorslot eq "SLOT")) {

                     $sender = cleanSender($sender);
                     my $signalArgument = extraArgumentFunctionName($signal);
                     $signal = extractFunctionName($signal);
                     $slot = extractFunctionName($slot);
                     my $localVariable;
                     if ( $sender =~ /^&/) {
                       $sender =~ s/^&//;
                       $localVariable = 1;
                     }
                     my $privateClass;
                     if ( $sender =~ /^d\-\>/) {
                       $sender =~ s/^d\-\>//;
                       $privateClass = 1;
                     }
                     if (defined $activateDebug) {
                        warn "With Argument and no receiver: SENDER: \'$sender\'  SIGNAL: \'$signal\' SLOT: \'$slot\' \n";
                     }

                     # => we don't have receiver => slot and signal will have same parent.
                     my $notpossible;

                     if ( $headerclassname eq "" ) {
                        $notpossible = "no current classname";
                     }

                     if ( defined $varname{$sender} ) {
                        $slot = "$headerclassname::$slot";
                        $signal = cast_overloaded_signal($varname{$sender}, $signalArgument, $signal);
                     } else {
                        $slot = "$headerclassname::$slot";
                        if ( $sender eq "this") {
                            $signal = cast_overloaded_signal($headerclassname, $signalArgument, $signal);
                        } elsif ( $sender eq "qApp") {
                          $signal = "&QApplication::$signal";
                        } elsif ( $sender eq "kapp") {
                          $signal = "&KApplication::$signal";
                        } elsif ( $sender =~ /button\(QDialogButtonBox::/) {
                          $signal = "&QPushButton::$signal";
                        } elsif ( $sender =~ /actionCollection\(\)\-\>action\b/) {
                           $signal = "&QAction::$signal";
                        } elsif ( $sender =~ /(.*)::self\(\)/) {
                          my $class = "&" . $1;
                          $signal = "$class::$signal";
                        } else {
                            if ( $sender =~ /(\w+)\.(.*)/  || $sender =~ /(\w+)\-\>(.*)/) {
                              my $uivariable = $1;
                              my $varui = $2;
                              if (defined $activateDebug) {
                                 warn "With Argument and no receiver: UI VARIABLE :$uivariable: variable name :$varui\n";
                              }
                              if (defined $localuiclass{$uivariable} ) {
                                if (defined $activateDebug) {
                                   warn "With Argument and no receiver: variable defined  $varui\n";
                                }

                                if ( defined $varname{$varui} ) {
                                   $signal = cast_overloaded_signal($varname{$varui}, $signalArgument, $signal);
 
                                  warn "vartype found $varname{$varui} \n";
                                } else {
                                  $notpossible = "unknown variable $varui";
                                }
                              } else {
                                $notpossible = "unknown class for $uivariable";
                              }
                            } else {
                                $notpossible = "unparsed sender $sender";
                            }
                        }
                      }
                      if (not defined $notpossible) {
                          $notpossible = checkOverloadedSlot($slot);
                      }
                      if (not defined $notpossible) {
                          $notpossible = checkPrivateSlot($slot);
                      }
                      if (not defined $notpossible) {
                        if ( defined $privateClass ) {
                            $sender = "d->" . $sender;
                        }
                        if ( defined $localVariable) {
                            $sender = "&" . $sender;
                        }
                        my $receiver = "this";
                        $_ = rewriteConnectFunction($indent, $sender, $signal, $receiver, $slot, $lastArgument);
                        undef $toorig;
                      } else {
                         my $line = $_;
                         chomp $line;
                         warn "$file : line $currentLine : Can not convert \'$line\' because $notpossible\n";
                         if (defined $toorig) {
                              $_ = $toorig;
                              undef $toorig;
                         }
                      }
                  }
              }
           }
        } else {
          if ( /^(\s*(?:[\-\>:\w]+)?)connect\b\s*/) {
             warn "It's perhaps a multi line " . $_ . "\n";
             $tojoin = $_;
             $toorig = $_;
             $_ = ""; 
          } 
        }
        $currentLine++;
        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
    }
}

functionUtilkde::diffFile( "@ARGV" );
