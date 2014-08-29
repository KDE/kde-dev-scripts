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
    warn "ClassName \'$classname\' Argument \'$argument\' Function \'$function\'\n";
    if (($classname eq "QCompleter") and ($argument eq "QString") and ($function eq "activated")) {
       return "static_cast<void (QCompleter::*)(const QString&)>(&QCompleter::activated)";
    } elsif (($classname eq "KNotification") and ($argument eq "(uint)") and ($function eq "activated")) {
       return "static_cast<void (KNotification::*)(unsigned int)>(&KNotification::activated)";    
    } elsif (($classname eq "KComboBox") ) {
       if (($argument eq "(int)") and ($function eq "activated")) {
          return "static_cast<void (KComboBox::*)(int)>(&KComboBox::activated)";
       } elsif (($argument eq "(int)") and ($function eq "currentIndexChanged")) {
          return "static_cast<void (KComboBox::*)(int)>(&KComboBox::currentIndexChanged)";
       }
    }
    return "";
}

sub initVariables
{
    %varname = ();
    $headerclassname = "";
    $numberOfClassName=0;
    %uiclassname = ();
    %localuiclass = ();
}

sub addToVarName
{
    my ($classname, $var) = @_;
    #warn "$file Found variable: classname:\'$classname\', variable: \'$var\'\n";
           
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

sub cleanSender
{
    my ($var) = @_;
    $var =~ s/^\(//;
    $var = functionUtilkde::cleanSpace($var);
    return $var;
}

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
    initVariables();
    # Search all ui file and parse them
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
        if (/Ui::(\w+)\s+(\w+);/ || /Ui::(\w+)\s*\*\s*(\w+);/) {
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

    # Parse cpp file
    my $modified;
    my %varnamewithpointer = ();
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        # private class in cpp file.
        if (/Ui::(\w+)\s+(\w+);/ || /Ui::(\w+)\s*\*\s*(\w+);/) {
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
           my ($sender, $signal, $receiver, $slot, $after);
           my $connectArgument_regexp = qr/
                                 ^([^,]*)\s*                 # (1) sender
                                 ,\s*SIGNAL\s*([^,]*)\s*     # (2) signal
                                 ,\s*([^,]*)                 # (3) receiver
                                 ,\s*SLOT\s*([^,]*)          # (4) slot
                                 (.*)$                       # (5) after
                                 /x;
           if ( ($sender, $signal, $receiver, $slot, $after) = $argument =~ $connectArgument_regexp) {
              #warn "Without arguments: SENDER: \'$sender\'  SIGNAL: \'$signal\' RECEIVER: \'$receiver\' SLOT: \'$slot\' \n";
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
                  my $overloadFound;
                  if ( defined $varname{$sender} ) {
                    my $overloadResult = overload($varname{$sender}, $signalArgument, $signal);
                    if (not $overloadResult eq "") {
                       $signal = $overloadResult;
                       $overloadFound = 1;
                    } else {
                       $signal = "$varname{$sender}::$signal";
                    }
                  } elsif ( defined $varnamewithpointer{$sender} ) {
                    $signal = "$varnamewithpointer{$sender}::$signal";
                    $classWithQPointer = 1;
                  } elsif ( $sender eq "this") {
                    $signal = "$headerclassname::$signal";
                  } elsif ( $sender eq "qApp") {
                     $signal = "QApplication::$signal";
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
                              $signal = "$varname{$varui}::$signal";
                           } else {
                             $notpossible = 1;
                           }
                       } elsif (defined $varname{$uivariable}) {
                           if ( defined $varname{$varui} ) {
                              my $overloadResult = overload($varname{$varui}, $signalArgument, $signal);
                              if (not $overloadResult eq "") {
                                 $signal = $overloadResult;
                                 $overloadFound = 1;
                              } else {
                                 $signal = "$varname{$varui}::$signal";
                              }

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
                     if (not defined $overloadFound) {
                        $signal = "&" . $signal;
                     }
                     $_ = $indent . "connect($sender, $signal, $receiver, &$slot);\n";
                  } else {
                     warn "Can not convert \'$_\' \n";
                  }
              }
              if (defined $activateDebug) {
                 warn "AFTER Without arguments: SENDER: \'$sender\'  SIGNAL: \'$signal\' RECEIVER: \'$receiver\' SLOT: \'$slot\' \n";
              }
           } else {
              warn "line actual : $_ argumen:\'$argument\'\n";
              my $connectArgument2_regexp = qr/
                                 ^([^,]*)\s*                                                       # (1) sender
                                 \s*,\s*SIGNAL\s*
                                 ${functionUtilkde::paren_begin}2${functionUtilkde::paren_end}     # (2) signal
                                 \s*,\s([^,]*)                                                     # (3) receiver
                                 \s*,\s*SLOT\s*
                                 ${functionUtilkde::paren_begin}4${functionUtilkde::paren_end}     # (4) slot
                                 (.*)$                                                             # (5) after
                                 /x;
              if ( ($sender, $signal, $receiver, $slot, $after) = $argument =~ $connectArgument2_regexp) {
                 $sender = cleanSender($sender);
                 my $signalArgument = extraArgumentFunctionName($signal);
                 my $overloadFound;

                 $signal = extractFunctionName($signal);
                 $slot = extractFunctionName($slot);

                 if (defined $activateDebug) {
                     warn "With Argument and receiver: SENDER: \'$sender\'  SIGNAL: \'$signal\' RECEIVER: \'$receiver\' SLOT: \'$slot\' \n";
                 }
                 my $notpossible;
                 if ( defined $varname{$sender} ) {
                    my $overloadResult = overload($varname{$sender}, $signalArgument, $signal);
                    if (not $overloadResult eq "") {
                       $signal = $overloadResult;
                       $overloadFound = 1;
                    } else {
                       $signal = "$varname{$sender}::$signal";
                    }
                 } elsif ( $sender eq "this") {
                   $signal = "$headerclassname::$signal";
                 } elsif ( $sender eq "qApp") {
                    $signal = "QApplication::$signal";
                 } elsif ( $sender =~ /button\(QDialogButtonBox::/) {
                   $signal = "QPushButton::$signal";
                 } elsif ( $sender =~ /(.*)::self\(\)/) {
                    my $class = $1;
                    $signal = "$class::$signal";
                 } else {
                    if ( $sender =~ /(\w+).(.*)/ || $sender =~ /(\w+)\-\>(.*)/) {
                       my $uivariable = $1;
                       my $varui = $2;
                       #warn "UI VARIABLE :$uivariable\n";
                       if (defined $localuiclass{$uivariable} ) {
                           #warn "variable defined  $varui\n";
                           if ( defined $varname{$varui} ) {
                              #warn "vartype found $varname{$varui} \n";
                              $signal = "$varname{$varui}::$signal";
                           } else {
                             $notpossible = 1;
                           }
                       } else {
                         $notpossible = 1;
                       }
                    }
                  }
                  if (not defined $notpossible) {
                     if ( defined $varname{$receiver} ) {
                      $slot = "$varname{$receiver}::$slot";
                    } elsif ( $receiver eq "this") {
                      $slot = "$headerclassname::$slot";
                    } else {
                      $notpossible = 1;
                    }
                  }
                  if (not defined $notpossible) {
                     if (not defined $overloadFound) {
                        $signal = "&" . $signal;
                     }
                     $_ = $indent . "connect($sender, $signal, $receiver, &$slot);\n";
                  } else {
                     warn "Can not convert \'$_\' \n";
                  }
              } else {
                my $connectArgument2_regexp = qr/
                                 ^([^,]*)\s*                                                       # (1) sender
                                 \s*,\s*SIGNAL\s*
                                 ${functionUtilkde::paren_begin}2${functionUtilkde::paren_end}     # (2) signal
                                 \s*,\s*SLOT\s*
                                 ${functionUtilkde::paren_begin}3${functionUtilkde::paren_end}     # (3) slot
                                 (.*)$                                                             # (4) after
                                 /x;
                if ( ($sender, $signal, $slot, $after) = $argument =~ $connectArgument2_regexp) {
                   $sender = cleanSender($sender);
                   my $signalArgument = extraArgumentFunctionName($signal);
                   my $overloadFound;
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
                      my $overloadResult = overload($varname{$sender}, $signalArgument, $signal);
                      if (not $overloadResult eq "") {
                         $signal = $overloadResult;
                         $overloadFound = 1;
                      } else {
                         $signal = "$varname{$sender}::$signal";
                      }
                   } else {
                      $slot = "$headerclassname::$slot";
                      if ( $sender eq "this") {
                        $signal = "$headerclassname::$signal";
                      } elsif ( $sender eq "qApp") {
                        $signal = "QApplication::$signal";
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
                                warn "vartype found $varname{$varui} \n";
                                $signal = "$varname{$varui}::$signal";
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
                      if (not defined $overloadFound) {
                        $signal = "&" . $signal;
                      }

                      $_ = $indent . "connect($sender, $signal, this, &$slot);\n";
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
