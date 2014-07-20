#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# convert KDialog to QDialog
# find -iname "*.cpp"|xargs kde-dev-scripts/kf5/convert-kascii.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;
our $paren_begin = '(\((?:(?>[^()]+)|(?';
our $paren_end = '))*\))';

my %dialogButtonType = (
   "Ok" => "QDialogButtonBox::Ok",
   "KDialog::Ok" => "QDialogButtonBox::Ok",
   "Cancel" => "QDialogButtonBox::Cancel",
   "KDialog::Cancel" => "QDialogButtonBox::Cancel",
   "Help" => "QDialogButtonBox::Help",
   "KDialog::Help" => "QDialogButtonBox::Help",
   "Default" => "QDialogButtonBox::RestoreDefaults",
   "KDialog::Default" => "QDialogButtonBox::RestoreDefaults",
   "Try" => "QDialogButtonBox::Retry",
   "KDialog::Try" => "QDialogButtonBox::Retry",
   "Close" => "QDialogButtonBox::Close",
   "KDialog::Close" => "QDialogButtonBox::Close",
   "No" => "QDialogButtonBox::No",
   "KDialog::No" => "QDialogButtonBox::No",
   "Yes" => "QDialogButtonBox::Yes",
   "KDialog::Yes" => "QDialogButtonBox::Yes",
   "Reset" => "QDialogButtonBox::Reset",
   "KDialog::Reset" => "QDialogButtonBox::Reset",
   "None" => "QDialogButtonBox::NoButton",
   "KDialog::None" => "QDialogButtonBox::NoButton"
);


foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my %varname = ();
    my $hasUser1Button;
    my $hasUser2Button;
    my $hasUser3Button;
    my $hasOkButton;
    my $needQDialogButtonBox;
    my $needQBoxLayout;
    my $hasMainWidget;
    my @l = map {
        my $orig = $_;
        my $regexp = qr/
          ^(\s*)           # (1) Indentation
          setMainWidget\s*\(
          (\w+)            # (2) variable name
          \);/x; # /x Enables extended whitespace mode
        if (my ($left, $var) = $_ =~ $regexp) {
           warn "setMainWidget found :$left $var\n";
           $_ = $left . "QVBoxLayout *mainLayout = new QVBoxLayout;\n";
           $_ .= $left . "setLayout(mainLayout);\n";
           $_ .= $left . "mainLayout->addWidget($var);\n";
           $varname{$var} = $var;
        }
        my $widget_regexp = qr/
           ^(\s*)            # (1) Indentation
           (.*?)             # (2) Possibly "Classname *" (the ? means non-greedy)
           (\w+)             # (3) variable name
           \s*=\s*           #     assignment
           new\s+            #     new
           (\w+)\s*          # (4) classname
           ${paren_begin}5${paren_end}  # (5) (args)
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $var, $classname, $args) = $_ =~ $widget_regexp) {
           # Extract last argument
           #print STDERR "left=$left var=$var classname=$classname args=$args\n";
           my $extract_parent_regexp = qr/
            ^\(
            (?:.*?)                # args before the parent (not captured, not greedy)
            \s*(\w+)\s*            # (1) parent 
            (?:,\s*\"[^\"]*\"\s*)? # optional: object name
             \)$
            /x; # /x Enables extended whitespace mode      
           if (my ($lastArg) = $args =~ $extract_parent_regexp) {
              print STDERR "extracted parent=" . $lastArg . "\n";
              my $mylayoutname = $varname{$lastArg};
              if (defined $mylayoutname) {
                  $_ .= $indent . "mainLayout->addWidget(" . $var . ");" . "\n";
               }
           } else {
              #warn $functionUtilkde::current_file . ":" . $functionUtilkde::current_line . ": couldn't extract last argument from " . $args . "\n";
           }
        }
        my $regexpMainWidget =qr/
         ^(\s*)           # (1) Indentation
         (.*?)            # (2) Possibly "Classname *" (the ? means non-greedy)
         \(\s*mainWidget\(\)
         \s*\);/x; # /x Enables extended whitespace mode
        if ( my ($left, $widget) = $_ =~ $regexpMainWidget) {
           if (not defined $hasMainWidget) {
             $_ = $left . "QWidget *mainWidget = new QWidget(this);\n";
             $_ .= $left . "QVBoxLayout *mainLayout = new QVBoxLayout;\n";
             $_ .= $left . "setLayout(mainLayout);\n";
             $_ .= $left . "mainLayout->addWidget(mainWidget);\n";
             $_ .= $left . "$widget(mainWidget);\n";
           } else {
             $_ = $left . "$widget(mainWidget);\n";
           }
           warn "found mainWidget \n";
           $needQBoxLayout = 1;
           $hasMainWidget = 1;
        }

        my $regexpSetButtons = qr/
          ^(\s*)           # (1) Indentation
          setButtons\s*\(
          (.*?)            # (2) variable name
          \);/x; # /x Enables extended whitespace mode
        if (my ($left, $var) = $_ =~ $regexpSetButtons) {
           #Remove space in enum
           $var =~ s, ,,g;
           $needQDialogButtonBox = 1;
           warn "setButtons found : $var\n";
           my @listButton = split(/\|/, $var);
           my @myNewDialogButton; 
           warn " list button found @listButton\n";
           if ( "Ok" ~~ @listButton || "KDialog::Ok" ~~ @listButton )  {
              push @myNewDialogButton , "QDialogButtonBox::Ok";
              $hasOkButton = 1;
           }
           if ( "Cancel" ~~ @listButton || "KDialog::Cancel" ~~ @listButton)  {
              push @myNewDialogButton , "QDialogButtonBox::Cancel";
           }
           if ( "Help" ~~ @listButton || "KDialog::Help" ~~ @listButton)  {
              push @myNewDialogButton , "QDialogButtonBox::Help";
           }
           if ( "Default" ~~ @listButton || "KDialog::Default" ~~ @listButton)  {
              push @myNewDialogButton , "QDialogButtonBox::RestoreDefaults";
           }
           if ( "Try" ~~ @listButton || "KDialog::Try" ~~ @listButton)  {
              push @myNewDialogButton , "QDialogButtonBox::Retry";
           }
           if ( "Close" ~~ @listButton || "KDialog::Close" ~~ @listButton)  {
              push @myNewDialogButton , "QDialogButtonBox::Close";
           }
           if ( "No" ~~ @listButton || "KDialog::No" ~~ @listButton)  {
              push @myNewDialogButton , "QDialogButtonBox::No";
           }
           if ( "Yes" ~~ @listButton || "KDialog::Yes" ~~ @listButton)  {
              push @myNewDialogButton , "QDialogButtonBox::Yes";
           }
           if ( "Reset" ~~ @listButton || "KDialog::Reset" ~~ @listButton)  {
              push @myNewDialogButton , "QDialogButtonBox::Reset";
           }
           if ( "Details" ~~ @listButton || "KDialog::Details" ~~ @listButton)  {
              warn "DETAILS is not implemented. Need to reimplement it\n";
           }
           if ( "None" ~~ @listButton || "KDialog::None" ~~ @listButton)  {
              push @myNewDialogButton , "QDialogButtonBox::NoButton";
           }
           if ( "User1" ~~ @listButton || "KDialog::User1" ~~ @listButton)  {
              $hasUser1Button = 1;
           }
           if ( "User2" ~~ @listButton || "KDialog::User2" ~~ @listButton)  {
              $hasUser2Button = 1;
           }
           if ( "User3" ~~ @listButton || "KDialog::User3" ~~ @listButton)  {
              $hasUser3Button = 1;
           }
           my $resultList = join('|', @myNewDialogButton);
           warn " $resultList \n";
           $_ = $left . "//PORTING SCRIPT: Move QDialogButtonBox at the end of init of widget to add it in layout.\n";
           $_ .= $left . "QDialogButtonBox *buttonBox = new QDialogButtonBox($resultList);\n";
           if (not defined $hasMainWidget) {
             $_ .= $left . "QWidget *mainWidget = new QWidget(this);\n";
             $_ .= $left . "QVBoxLayout *mainLayout = new QVBoxLayout;\n";
             $_ .= $left . "setLayout(mainLayout);\n";
             $_ .= $left . "mainLayout->addWidget(mainWidget);\n";
             #$_ .= $left . "$widget(mainWidget);\n";
             $hasMainWidget = 1;
             $needQBoxLayout = 1;
           }

           if (defined $hasOkButton) {
              $_ .= $left . "QPushButton *okButton = buttonBox->button(QDialogButtonBox::Ok);\n";
              $_ .= $left . "okButton->setDefault(true);\n";
              $_ .= $left . "okButton->setShortcut(Qt::CTRL | Qt::Key_Return);\n";
           }

           if (defined $hasUser1Button) {
              $_ .= $left . "QPushButton *user1Button = new QPushButton;\n";
              $_ .= $left . "buttonBox->addButton(user1Button, QDialogButtonBox::ActionRole);\n";
           }
           if (defined $hasUser2Button) {
              $_ .= $left . "QPushButton *user2Button = new QPushButton;\n";
              $_ .= $left . "buttonBox->addButton(user2Button, QDialogButtonBox::ActionRole);\n";
           }
           if (defined $hasUser3Button) {
              $_ .= $left . "QPushButton *user3Button = new QPushButton;\n";
              $_ .= $left . "buttonBox->addButton(user3Button, QDialogButtonBox::ActionRole);\n";
           }

           $_ .= $left . "connect(buttonBox, SIGNAL(accepted()), this, SLOT(accept()));\n";
           $_ .= $left . "connect(buttonBox, SIGNAL(rejected()), this, SLOT(reject()));\n";
           $_ .= $left . "mainLayout->addWidget(buttonBox);\n";
           
           warn "WARNING we can't move this code at the end of constructor. Need to move it !!!!\n";
        }
        #TODO fix connect signal/slot
        if (/defaultClicked\(\)/) {
             s/connect\s*\(\s*this,/connect(buttonBox->button(QDialogButtonBox::RestoreDefaults),/;
             s/defaultClicked\(\)/clicked()/;
        }
        if (/okClicked\(\)/) {
             s/connect\s*\(\s*this,/connect(okButton,/;
             s/okClicked\(\)/clicked()/;
        }
        if (/cancelClicked\(\)/) {
             s/connect\s*\(\s*this,/connect(buttonBox->button(QDialogButtonBox::Cancel),/;
             s/cancelClicked\(\)/clicked()/;
        }
        if (/user1Clicked\(\)/) {
             s/connect\s*\(\s*this,/connect(user1Button,/;
             s/user1Clicked\(\)/clicked()/;
        }
        if (/user2Clicked\(\)/) {
             s/connect\s*\(\s*this,/connect(user2Button,/;
             s/user2Clicked\(\)/clicked()/;
        }
        if (/user3Clicked\(\)/) {
             s/connect\s*\(\s*this,/connect(user3Button,/;
             s/user3Clicked\(\)/clicked()/;
        }

        
        my $regexEnableButton = qr/
          ^(\s*)           # (1) Indentation
          enableButton\s*\(
          ${paren_begin}2${paren_end}  # (5) (args)
          \);/x; # /x Enables extended whitespace mode
        if (my ($left, $args) = $_ =~ $regexEnableButton) {
           warn "found enableButton $args\n";
           my $extract_args_regexp = qr/
                                 ^\(([^,]*)           # button
                                 ,\s*([^,]*)        # state
                                 (.*)$              # after
                                 /x;
           if ( my ($defaultButtonType, $state) = $args =~  $extract_args_regexp ) {
              $defaultButtonType =~ s, ,,g;
              $state =~ s, ,,g;
              $state =~ s,\),,g;
             if (defined $dialogButtonType{$defaultButtonType}) {
                if ( $defaultButtonType eq "Ok") {
                   $_ = $left . "okButton->setEnabled($state);\n";
                } else {
                   $_ = $left . "buttonBox->button($dialogButtonType{$defaultButtonType})->setEnabled($state);\n";
                }
             } else {
                if ($defaultButtonType eq "User1") {
                   $_ = $left . "user1Button\->setEnabled($state);\n";
                } elsif ($defaultButtonType eq "User2") {
                   $_ = $left . "user2Button\->setEnabled($state);\n";
                } elsif ($defaultButtonType eq "User3") {
                   $_ = $left . "user3Button\->setEnabled($state);\n";
                } else {
                   warn "Enable button: unknown or not supported \'$defaultButtonType\'\n";
                }
             }
             warn "Found enabled button \'$defaultButtonType\', state \'$state\'\n";
           }
        }

        my $regexDefaultButton = qr/
                               ^(\s*)           # (1) Indentation
                               setDefaultButton\s*\(
                               (.*)
                               \);/x; # /x Enables extended whitespace mode
        if ( my ($left, $defaultButtonType) = $_ =~ $regexDefaultButton ) {
           warn "Found default button type : $defaultButtonType\n";
           $defaultButtonType =~ s, ,,g;
           if (defined $dialogButtonType{$defaultButtonType}) {
              if ( $defaultButtonType eq "Ok") {
                 $_ = $left . "okButton->setDefault(true);\n";
              } else {
                 $_ = $left . "buttonBox->button($dialogButtonType{$defaultButtonType})->setDefault(true);\n";
              }
           } else {
              if ($defaultButtonType eq "User1") {
                 $_ = $left . "user1Button\->setDefault(true);\n";
              } elsif ($defaultButtonType eq "User2") {
                 $_ = $left . "user2Button\->setDefault(true);\n";
              } elsif ($defaultButtonType eq "User3") {
                 $_ = $left . "user3Button\->setDefault(true);\n";
              } else {
                warn "Default button type unknown or not supported \'$defaultButtonType\'\n";
              }
           }
        }

        my $regexButtonFocus = qr/
                               ^(\s*)           # (1) Indentation
                               setButtonFocus\s*\(
                               (.*)
                               \s*\);/x; # /x Enables extended whitespace mode
        if ( my ($left, $defaultButtonType) = $_ =~ $regexButtonFocus ) {
           $defaultButtonType =~ s, ,,g;
           warn "Found default button focus : $defaultButtonType\n";
           if (defined $dialogButtonType{$defaultButtonType}) {
              if ( $defaultButtonType eq "Ok") {
                 $_ = $left . "okButton->setFocus();\n";
              } else {
                 $_ = $left . "buttonBox->button($dialogButtonType{$defaultButtonType})->setFocus();\n";
              }
           } else {
              if ($defaultButtonType eq "User1") {
                 $_ = $left . "user1Button\->setFocus();\n";
              } elsif ($defaultButtonType eq "User2") {
                 $_ = $left . "user2Button\->setFocus();\n";                
              } elsif ($defaultButtonType eq "User3") {
                 $_ = $left . "user3Button\->setFocus();\n";
              } else {
                warn "Default button focus: unknown or not supported \'$defaultButtonType\'\n";
              }
           }
        }


        my $regexSetButtonText = qr/
          ^(\s*)           # (1) Indentation
          setButtonText\s*\(
          ${paren_begin}2${paren_end}  # (2) (args)
          \);/x; # /x Enables extended whitespace mode
        if (my ($left, $args) = $_ =~ $regexSetButtonText) {
           warn "found setButtonText $args\n";
           my $extract_args_regexp = qr/
                                 ^\(([^,]*)           # button
                                 ,\s*([^,]*)        # state
                                 (.*)$              # after
                                 /x;
           if ( my ($defaultButtonType, $text) = $args =~  $extract_args_regexp ) {
              $defaultButtonType =~ s, ,,g;
              $text =~ s, ,,g;
              $text =~ s,\),,g;
              if (defined $dialogButtonType{$defaultButtonType}) {
                 if ( $defaultButtonType eq "Ok") {
                    $_ = $left . "okButton->setText($text);\n";
                 } else {
                    $_ = $left . "buttonBox->button($dialogButtonType{$defaultButtonType})->setText($text);\n";
                }
              } else {
                 if ($defaultButtonType eq "User1") {
                    $_ = $left . "user1Button\->setText($text);\n";
                 } elsif ($defaultButtonType eq "User2") {
                    $_ = $left . "user2Button\->setText($text);\n";
                 } elsif ($defaultButtonType eq "User3") {
                    $_ = $left . "user3Button\->setText($text);\n";
                 } else {
                     warn "Set button Text: unknown or not supported \'$defaultButtonType\'\n";
                 }
              }
           }
        }

        my $regexSetButtonMenu = qr/
          ^(\s*)           # (1) Indentation
          setButtonMenu
          ${paren_begin}2${paren_end}  # (2) (args)
          /x; # /x Enables extended whitespace mode
        if (my ($left, $args) = $_ =~ $regexSetButtonMenu) {
           warn "found setButtonMenu $args\n";
           my $extract_args_regexp = qr/
                                 ^\(([^,]*)           # button
                                 ,\s*([^,]*)        # state
                                 (.*)$              # after
                                 /x;
           if ( my ($button, $menuName) = $args =~  $extract_args_regexp ) {
              $button =~ s, ,,g;
              $menuName =~ s, ,,g;
              $menuName =~ s,\),,g;
              warn "Found setButtonMenu: \'$button\', menu variable \'$menuName\'\n";
              if (defined $dialogButtonType{$button}) {
                 if ( $button eq "Ok" || $button eq "KDialog::Ok") {
                    $_ = $left . "okButton->setMenu($menuName);\n";
                 } else {
                    $_ = $left . "buttonBox->button($dialogButtonType{$button})->setMenu($menuName);\n";
                }
              } else {
                 if ($button eq "User1") {
                    $_ = $left . "user1Button\->setMenu($menuName);\n";
                 } elsif ($button eq "User2") {
                    $_ = $left . "user2Button\->setMenu($menuName);\n";
                 } elsif ($button eq "User3") {
                    $_ = $left . "user3Button\->setMenu($menuName);\n";
                 } else {
                     warn "Set Button Menu: unknown or not supported \'$button\'\n";
                 }
              }
           }
        }
        # TODO setButtonIcon
        # TODO setButtonGuiItem

        if (/KDialog::spacingHint/) {
           $_ = "//TODO PORT QT5 " .  $_;
        }
        if (/KDialog::marginHint/) {
           $_ = "//TODO PORT QT5 " .  $_;
        }

        if (/\bsetMainWidget\b/) {
           # remove setMainWidget doesn't exist now.
           $_ = "//PORTING: Verify that widget was added to mainLayout " . $_;
        }
        if (/\bshowButtonSeparator\b/) {
           # remove showButtonSeparator doesn't exist now.
           $_ = "";
        }
        

        my $regexButton = qr/
          ^(\s*)           # (1) Indentation
          button
          ${paren_begin}2${paren_end}  # (2) (args)
          /x; # /x Enables extended whitespace mode
        if (my ($left, $button) = $_ =~ $regexButton) {
           $button =~ s/\(//;
           $button =~ s/\)//;
           $button =~ s, ,,g;
           if (defined $dialogButtonType{$button}) {
              if ( $button eq "Ok" || $button eq "KDialog::Ok") {
                 s/button\s*\(\s*KDialog::Ok\s*\)/okButton/;
                 s/button\s*\(\s*Ok\s*\)/okButton/;
              } else {
                 s/button\s*\(\s*$button\s*\)/buttonBox->button($dialogButtonType{$button})/;
              }
           } else {
              if ($button eq "User1") {
                 s/button\s*\(\s*KDialog::User1\s*\)/user1Button/;
                 s/button\s*\(\s*User1\s*\)/user1Button/;

              } elsif ($button eq "User2") {
                 s/button\s*\(\s*KDialog::User2\s*\)/user1Button/;
                 s/button\s*\(\s*User2\s*\)/user1Button/;
              } elsif ($button eq "User3") {
                 s/button\s*\(\s*KDialog::User3\s*\)/user1Button/;
                 s/button\s*\(\s*User3\s*\)/user1Button/;
              } else {
                 warn "button(...): unknown or not supported \'$button\'\n";
              }
           }

           warn "Found button(...) with argument $button\n";
        }


        s/\bsetCaption\b/setWindowTitle/;
        s/\benableButtonOk\b/okButton->setEnabled/;
        s/\bKDialog\b/QDialog/g;
        s/\<KDialog\b\>/\<QDialog>/ if (/#include/);
        s/\<kdialog.h\>/\<QDialog>/ if (/#include/);
        s/\bsetInitialSize\b/resize/;
        s/button\s*\(\s*User2\s*\)/user2Button/;
        s/button\s*\(\s*User1\s*\)/user1Button/;
        s/button\s*\(\s*User3\s*\)/user3Button/;

        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open (my $OUT, ">", $file);
        print $OUT @l;
        close ($OUT);
        if (defined $needQDialogButtonBox) {
           functionUtilkde::addIncludeInFile($file, "QDialogButtonBox");
           # Need to add KConfigGroup because it was added by kdialog before
           functionUtilkde::addIncludeInFile($file, "KConfigGroup");
           functionUtilkde::addIncludeInFile($file, "QPushButton");
        }
        if (defined $needQBoxLayout) {
           functionUtilkde::addIncludeInFile($file, "QVBoxLayout");
        }
    }
}

functionUtilkde::diffFile( "@ARGV" );
