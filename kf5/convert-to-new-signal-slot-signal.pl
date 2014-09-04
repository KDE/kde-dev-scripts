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

sub addData
{
    my ($var) = @_;
    $var = $var . ".data()";
    return $var;
}


sub overload
{
    my ($classname, $argument, $function) = @_;
    my $result;
    warn "ClassName \'$classname\' Argument \'$argument\' Function \'$function\'\n";
    if (($classname eq "QCompleter") and ($argument eq "(QString)") and ($function eq "activated")) {
       $result = "static_cast<void (QCompleter::*)(const QString&)>(&QCompleter::activated)";
    } elsif (($classname eq "KNotification") and ($argument eq "(uint)") and ($function eq "activated")) {
       $result = "static_cast<void (KNotification::*)(unsigned int)>(&KNotification::activated)";    
    } elsif (($classname eq "KComboBox") ) {
       if (($argument eq "(int)") and ($function eq "activated")) {
          $result = "static_cast<void (KComboBox::*)(int)>(&KComboBox::activated)";
       } elsif (($argument eq "(QString)") and ($function eq "activated")) {
          $result = "static_cast<void (KComboBox::*)(const QString &)>(&KComboBox::activated)";
       } elsif (($argument eq "(int)") and ($function eq "currentIndexChanged")) {
          $result = "static_cast<void (KComboBox::*)(int)>(&KComboBox::currentIndexChanged)";
       } elsif (($argument eq "(QString)") and ($function eq "currentIndexChanged")) {
          $result = "static_cast<void (KComboBox::*)(const QString &)>(&KComboBox::currentIndexChanged)";
       }
    } elsif (($classname eq "QComboBox") ) {
       if (($argument eq "(int)") and ($function eq "activated")) {
          $result = "static_cast<void (QComboBox::*)(int)>(&QComboBox::activated)";
       } elsif (($argument eq "(QString)") and ($function eq "activated")) {
          $result = "static_cast<void (QComboBox::*)(const QString &)>(&QComboBox::activated)";
       } elsif (($argument eq "(int)") and ($function eq "currentIndexChanged")) {
          $result = "static_cast<void (QComboBox::*)(int)>(&QComboBox::currentIndexChanged)";
       } elsif (($argument eq "(QString)") and ($function eq "currentIndexChanged")) {
          $result = "static_cast<void (QComboBox::*)(const QString &)>(&QComboBox::currentIndexChanged)";
       }

    } elsif ($classname eq "QButtonGroup") {
       if (($argument eq "(int)") and ($function eq "buttonReleased")) {
          $result = "static_cast<void (QButtonGroup::*)(int)>(&QButtonGroup::buttonReleased)";
       } elsif (($argument eq "(QAbstractButton*)") and ($function eq "buttonReleased")) {
          $result = "static_cast<void (QButtonGroup::*)(QAbstractButton*)>(&QButtonGroup::buttonReleased)";       
       } elsif (($argument eq "(QAbstractButton*)") and ($function eq "buttonClicked")) {
          $result = "static_cast<void (QButtonGroup::*)(QAbstractButton*)>(&QButtonGroup::buttonClicked)";
       } elsif (($argument eq "(int)") and ($function eq "buttonClicked")) {
          $result = "static_cast<void (QButtonGroup::*)(int)>(&QButtonGroup::buttonClicked)";
       } elsif (($argument eq "(QAbstractButton*)") and ($function eq "buttonPressed")) {
          $result = "static_cast<void (QButtonGroup::*)(QAbstractButton*)>(&QButtonGroup::buttonPressed)";
       } elsif (($argument eq "(int)") and ($function eq "buttonPressed")) {
          $result = "static_cast<void (QButtonGroup::*)(int)>(&QButtonGroup::buttonPressed)";
       } elsif (($argument eq "(QAbstractButton*)") and ($function eq "buttonToggled")) {
          $result = "static_cast<void (QButtonGroup::*)(QAbstractButton*)>(&QButtonGroup::buttonToggled)";
       } elsif (($argument eq "(int)") and ($function eq "buttonToggled")) {
          $result = "static_cast<void (QButtonGroup::*)(int)>(&QButtonGroup::buttonToggled)";
       }
    } elsif (($classname eq "QSpinBox") or ($classname eq "KPluralHandlingSpinBox")) {
       if (($argument eq "(int)") and ($function eq "valueChanged")) {
          $result = "static_cast<void ($classname" . "::*)(int)>(&" . "$classname" . "::valueChanged)";
       } elsif (($argument eq "(QString)") and ($function eq "valueChanged")) {
          $result = "static_cast<void ($classname" . "::*)(const QString &)>(&" . "$classname" . "::valueChanged)";
       }
    } elsif (($classname eq "KSelectAction") or ($classname eq "KFontAction")) {
       if (($argument eq "(QString)") and ($function eq "triggered")) {
          $result = "static_cast<void ($classname" . "::*)(const QString &)>(&" . "$classname" . "::triggered)";
       } elsif (($argument eq "(int)") and ($function eq "triggered")) {
          $result = "static_cast<void ($classname" . "::*)(int)>(&" . "$classname" . "::triggered)";
       } elsif(($argument eq "(QAction*)") and ($function eq "triggered")) {
          $result = "static_cast<void ($classname" . "::*)(QAction*)>(&" . "$classname" . "::triggered)";
       }
    } elsif ($classname eq "KUrlLabel") {
      if (($argument eq "(QString)") and ($function eq "leftClickedUrl")) {
          $result = "static_cast<void (KUrlLabel::*)(const QString &)>(&KUrlLabel::leftClickedUrl)";
      } elsif (($argument eq "()") and ($function eq "leftClickedUrl")) {
          $result = "static_cast<void (KUrlLabel::*)()>(&KUrlLabel::leftClickedUrl)";
      } elsif (($argument eq "(QString)") and ($function eq "rightClickedUrl")) {
          $result = "static_cast<void (KUrlLabel::*)(const QString &)>(&KUrlLabel::rightClickedUrl)";
      } elsif (($argument eq "()") and ($function eq "rightClickedUrl")) {
          $result = "static_cast<void (KUrlLabel::*)()>(&KUrlLabel::rightClickedUrl)";
      } elsif (($argument eq "(QString)") and ($function eq "rightClickedUrl")) {
          $result = "static_cast<void (KUrlLabel::*)(const QString &)>(&KUrlLabel::rightClickedUrl)";
      } elsif (($argument eq "()") and ($function eq "rightClickedUrl")) {
          $result = "static_cast<void (KUrlLabel::*)()>(&KUrlLabel::rightClickedUrl)";
      }
    } elsif ($classname eq "QTcpSocket") {
      if (($argument eq "(QAbstractSocket::SocketError)") and ($function eq "error")) {
          $result = "static_cast<void (QTcpSocket::*)(QAbstractSocket::SocketError)>(&QTcpSocket::error)";
      } elsif (($argument eq "()") and ($function eq "error")) {
          $result = "static_cast<void (QTcpSocket::*)()>(&QTcpSocket::error)";
      }
    } elsif ($classname eq "Akonadi::ChangeRecorder") {
      if ($function eq "collectionChanged") {
         if ($argument eq "(Akonadi::Collection)") {
            $result = "static_cast<void ($classname" . "::*)(const Akonadi::Collection &)>(&" . "$classname" . "::$function)";
         } elsif ($argument eq "(Akonadi::Collection,QSet<QByteArray>)") {
            $result = "static_cast<void ($classname" . "::*)(const QSet<QByteArray &)>(&" . "$classname" . "::$function)";         
         }
      }
    }
    
    if ( defined $result ) {
       return $result;
    } else {
       $result = "&" . "$classname::$function";
       return $result;
    }
}

# initialize variable before to parse new file
sub initVariables
{
    %varname = ();
    $headerclassname = "";
    $numberOfClassName=0;
    %uiclassname = ();
    %localuiclass = ();
}

# add new variable with its type.
sub addToVarName
{
    my ($classname, $var) = @_;           
    if (not $classname eq ":" and not $classname eq "return") { 
      #If we found variable in header don't overwrite it
      if (not defined $varname{$var}) {
          $varname{$var} = ${classname}; 
          if (defined $activateDebug) {
              warn "new variable added: \'$var\' className :\'$classname\'\n";
          }
      }
   }
}

# Clean sender variable
sub cleanSender
{
    my ($var) = @_;
    $var =~ s/^\(//;
    $var = functionUtilkde::cleanSpace($var);
    return $var;
}

# extract argument from signal
sub extraArgumentFunctionName
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
sub extractFunctionName
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

foreach my $file (@ARGV) {
    
    # 1) initialize variable before to parse file
    initVariables();
    
    # 2) Search all ui file and parse them
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
        $_;
      } <$FILE>
    }

    # 3) read header and parse it.
    my $header = functionUtilkde::headerName($file);
    warn "Parse header file: $header \n";
    # parse header file
    open(my $HEADERFILE, "<", $header) or warn "We can't open file $header:$!\n";
    my @lheader = map {
        my $orig = $_;
        my $regexp = qr/
           ^(\s*)                        # (1) Indentation
           (.*?)                         # (2) Possibly "Classname *" (the ? means non-greedy)
           \*\s*([:\w]+)\s*                 # (3) variable name
           ;/x; # /x Enables extended whitespace mode
        if (my ($indent, $classname, $var) = $_ =~ $regexp) {
           $classname = functionUtilkde::cleanSpace($classname);
            if (defined $activateDebug) {
               warn "$header file: found classname \'$classname\' variable: \'$var\' indent \'indent\'\n";
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
           $numberOfClassName++;
        }
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
        if ( /([:\w]+)\s+(\w+);/) {
           my $classname = $1;
           my $var = $2;
           addToVarName($classname, $var);
        }

        # Foo toto = 
        if ( /.*([:\w]+)\s+(\w+)\s*=/) {
           my $classname = $1;
           my $var = $2;
           addToVarName($classname, $var);
       }

        # Foo toto(...)
        if ( /.*([:\w]+)\s+(\w+)\(/) {
           my $classname = $1;
           my $var = $2;
           addToVarName($classname, $var);
        }

        # Private* const d;
        if (/\s*(\w+)\s*\*\s*const\s+(\w+);/ ) {
           my $classname = $1;
           my $var = $2;
           addToVarName($classname, $var);
        }

        $_;
    } <$HEADERFILE>;

    warn "We have $numberOfClassName class in $header\n";

    
    # 4) Parse cpp file
    my $modified;
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
           if (not defined $varname{$var}) {
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


        # Foo toto;        
        if ( /^\s*([:\w]+)\s+(\w+);/) {
           my $classname = $1;
           my $var = $2;
           addToVarName($classname, $var);
        }
        # Foo *toto = 
        if ( /^\s*([:\w]+)\s*\*\s*(\w+)\s*=/) {
           my $classname = $1;
           my $var = $2;
           addToVarName($classname, $var);
        }


        # Foo toto = 
        if ( /^\s*([:\w]+)\s+(\w+)\s*=/) {
           my $classname = $1;
           my $var = $2;
           addToVarName($classname, $var);
        }

        # Foo toto(...)
        if ( /^\s*([:\w]+)\s+(\w+)\(/) {
           my $classname = $1;
           my $var = $2;
           addToVarName($classname, $var);
        }

       #AttachmentControllerBase *const q;
        if (/\s*(\w+)\s*\*const\s+(\w+);/ ) {
           my $classname = $1;
           my $var = $2;
           addToVarName($classname, $var);
        }



        my $regexpConnect = qr/
          ^(\s*(?:[\-\>:\w]+)?)           # (1) Indentation
          connect
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

                if ( (defined $varname{$sender}) and (defined $varname{$receiver}) ) {
                    $signal = "$varname{$sender}::$signal";
                    $slot = "$varname{$receiver}::$slot";
                    if ( defined $localSenderVariable) {
                       $sender = "&" . $sender;
                    }
                    if ( defined $localReceiverVariable) {
                       $receiver = "&" . $receiver;
                    }
                    $_ = $indent . "connect($sender, &$signal, $receiver, &$slot);\n";
                } else {
                  my $notpossible;
                  my $classWithQPointer;
                  my $receiverWithQPointer;
                  if ( defined $varname{$sender} ) {
                    $signal = overload($varname{$sender}, $signalArgument, $signal);
                  } elsif ( defined $varnamewithpointer{$sender} ) {
                    $signal = "$varnamewithpointer{$sender}::$signal";
                    $classWithQPointer = 1;
                  } elsif ( $sender eq "this") {
                    $signal = "$headerclassname::$signal";
                  } elsif ( $sender eq "qApp") {
                     $signal = "QApplication::$signal";
                  } elsif ( $sender eq "kapp") {
                     $signal = "KApplication::$signal";
                  } elsif ( $sender =~ /button\(QDialogButtonBox::/) {
                    $signal = "QPushButton::$signal";
                  } elsif ( $sender =~ /(.*)::self\(\)/) {
                    my $class = $1;
                    $signal = "$class::$signal";
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
                              $signal = overload($varname{$varui}, $signalArgument, $signal);
                           } else {
                             $notpossible = 1;
                           }
                       } elsif (defined $varname{$uivariable}) {
                           if ( defined $varname{$varui} ) {
                              $signal = overload($varname{$varui}, $signalArgument, $signal);

                              if (defined $activateDebug) {
                                 warn "vartype found $varname{$varui} \n";
                              }
                           }
                       } else {
                         $notpossible = 1;
                       }
                    }
                  }
                  if (not defined $notpossible) {

                    if ( defined $varname{$receiver} ) {
                      $slot = "$varname{$receiver}::$slot";
                    } elsif ( defined $varnamewithpointer{$receiver} ) {
                      $slot = "$varnamewithpointer{$receiver}::$slot";
                      $receiverWithQPointer = 1;
                    } elsif ( $receiver eq "this") {
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
                                 $notpossible = 1;
                              }
                          } else {
                            $notpossible = 1;
                          }
                       }
                    }
                  }
                  if (not defined $notpossible) {
                     if ( defined $localSenderVariable) {
                        $sender = "&" . $sender;
                     }
                     if ( defined $localReceiverVariable) {
                        $receiver = "&" . $receiver;
                     }
                     if ( defined $classWithQPointer) {
                        $sender = addData($sender);
                     }
                     if (defined $receiverWithQPointer) {
                        $receiver = addData($receiver);
                     }
                     if (defined $lastArgument) {
                        # lastArgument has ')'
                        $_ = $indent . "connect($sender, $signal, $receiver, &$slot, $lastArgument;\n";
                     } else {
                        $_ = $indent . "connect($sender, $signal, $receiver, &$slot);\n";
                     }
                  } else {
                     warn "Can not convert \'$_\' \n";
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
                     if ( defined $varname{$sender} ) {
                        $slot = "$headerclassname::$slot";
                        $signal = overload($varname{$sender}, $signalArgument, $signal);
                     } else {
                        $slot = "$headerclassname::$slot";
                        if ( $sender eq "this") {
                          $signal = "$headerclassname::$signal";
                        } elsif ( $sender eq "qApp") {
                          $signal = "QApplication::$signal";
                        } elsif ( $sender eq "kapp") {
                          $signal = "KApplication::$signal";
                        } elsif ( $sender =~ /button\(QDialogButtonBox::/) {
                          $signal = "QPushButton::$signal";
                        } elsif ( $sender =~ /(.*)::self\(\)/) {
                          my $class = $1;
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
                                   $signal = overload($varname{$varui}, $signalArgument, $signal);
 
                                  warn "vartype found $varname{$varui} \n";
                                } else {
                                  $notpossible = 1;
                                }
                            } else {
                              $notpossible = 1;
                            }
                          }
                        }
                      }
                      if (not defined $notpossible) {
                        if ( defined $privateClass ) {
                            $sender = "d->" . $sender;
                        }
                        if ( defined $localVariable) {
                            $sender = "&" . $sender;
                        }
                        if (defined $lastArgument) {
                          $_ = $indent . "connect($sender, $signal, this, &$slot, $lastArgument;\n";
                        } else {
                          $_ = $indent . "connect($sender, $signal, this, &$slot);\n";
                        }
                      } else {
                         warn "Can not convert \'$_\' \n";
                      }
                  }
              }
           }
        }

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
