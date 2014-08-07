#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# Convert to new signal/slot syntax
# it's totally experimental
# find -iname "*.cpp" |xargs kde-dev-scripts/kf5/convert-to-new-signal-slot-signal.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

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
    my %varname = ();
    my $headerclassname;
    my $numberOfClassName=0;

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
           warn "$header file: found classname \'$classname\' variable: \'$var\'\n";
           $varname{$var} = ${classname};
        }

        # Search header class name
        if (/class\s*(?:\w+EXPORT|\w+DEPRECATED)?\s*(\w+)\s*:\s*public\s*(.*)/) {
           my $class = $1;
           my $parentClass = $2;
           warn "FOUND Class \'$class\' parentClass: \'$parentClass\' $_\n";
           $headerclassname = $class;
           $numberOfClassName++;
        }
        $_;
    } <$HEADERFILE>;

    warn "We have $numberOfClassName class in $header\n";

    # Parse cpp file
    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

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
           $varname{$var} = ${classname};
           warn "$file: cpp file: found classname \'$classname\' variable: \'$var\'\n";
        }
        if (/(\w+)\s*\*(\w+)\s*=.*addAction\s*\(/) {
           my $classname = $1;
           my $var = $2;
           $varname{$var} = ${classname};
        }


        my $regexpConnect = qr/
          ^(\s*)           # (1) Indentation
          connect
          ${functionUtilkde::paren_begin}2${functionUtilkde::paren_end}  # (2) (args)         
          ;/x; # /x Enables extended whitespace mode
        if (my ($indent, $argument) = $_ =~ $regexpConnect ) {
           #warn "ARGUMENT $argument\n";
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
              $sender =~ s/^\(//;
              $sender = functionUtilkde::cleanSpace($sender);
              $signal = extractFunctionName($signal);
              $slot = extractFunctionName($slot);
              if ( (defined $varname{$sender}) and (defined $varname{$receiver}) ) {
                  $signal = "$varname{$sender}::$signal";
                  $slot = "$varname{$receiver}::$slot";
                  $_ = $indent . "connect($sender, &$signal, $receiver, &$slot);\n";
              } else {
                  my $notpossible;
                  if ( defined $varname{$sender} ) {
                    $signal = "$varname{$sender}::$signal";
                  } elsif ( $sender eq "this") {
                    $signal = "$headerclassname::$signal";
                  } else {
                    $notpossible = 1;
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
                     $_ = $indent . "connect($sender, &$signal, $receiver, &$slot);\n";
                  } else {
                     warn "Can not convert \'$_\' \n";
                  }
              }
 
              warn "AFTER Without arguments: SENDER: \'$sender\'  SIGNAL: \'$signal\' RECEIVER: \'$receiver\' SLOT: \'$slot\' \n";
           } else {
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
                 warn "With Argument and receiver: SENDER: \'$sender\'  SIGNAL: \'$signal\' RECEIVER: \'$receiver\' SLOT: \'$slot\' \n";
                  my $notpossible;
                  if ( defined $varname{$sender} ) {
                    $signal = "$varname{$sender}::$signal";
                  } elsif ( $sender eq "this") {
                    $signal = "$headerclassname::$signal";
                  } else {
                    $notpossible = 1;
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
                     $_ = $indent . "connect($sender, &$signal, $receiver, &$slot);\n";
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
                   $sender =~ s/^\(//;
                   $sender = functionUtilkde::cleanSpace($sender);
                   $signal = extractFunctionName($signal);
                   $slot = extractFunctionName($slot);

                   warn "With Argument and no receiver: SENDER: \'$sender\'  SIGNAL: \'$signal\' SLOT: \'$slot\' \n";
                   if ( defined $varname{$sender} ) {
                      $slot = "$headerclassname::$slot";
                      my $notpossible;
                      if ( defined $varname{$sender} ) {
                         $signal = "$varname{$sender}::$signal";
                      } elsif ( $sender eq "this") {
                        $signal = "$headerclassname::$signal";
                      } else {
                        $notpossible = 1;
                      }
                      if (not defined $notpossible) {
                         $_ = $indent . "connect($sender, &$signal, this, &$slot);\n";
                      } else {
                         warn "Can not convert \'$_\' \n";
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
