#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2017)
# Use qoverload (qt5.7)
# it's totally experimental
# find -iname "*.cpp" |xargs kde-dev-scripts/kf5/convert-to_qoverload.pl

use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

my $activateDebug = 1;



foreach my $file (@ARGV) {


    my $currentLine = 1;

    # 4) Parse cpp file
    my $modified;
    my $tojoin;
    my $toorig;
    my %varnamewithpointer = ();
    open( my $FILE, "<", $file ) or warn "We can't open file $file:$!\n";
    my @l = map {
        my $orig = $_;

        # Verify comment
        if ( defined $tojoin ) {

            $toorig .= $_;

            $tojoin =~ s/\s*\n$//;    # remove any trailing space
            $_ =~ s/^\s*/ /;          # replace indent with single space
            $_ = $tojoin . $_;
            warn "look at end ? \'$_\'\n";
            if ( /;\s*$/ || /;\s*\/\*\.*\*\// || /;\s*\n$/ ) {
                undef $tojoin;
            }
        }

        my $regexpConnect = qr/
          ^(\s*(?:[\-\>:\w]+)?)           # (1) Indentation, optional classname or variable name
          connect\s*
          ${functionUtilkde::paren_begin}2${functionUtilkde::paren_end}  # (2) (args)         
          ;/x;                        # /x Enables extended whitespace mode
        if ( my ( $indent, $argument ) = $_ =~ $regexpConnect ) {
            if ( defined $activateDebug ) {
                warn "ARGUMENT $argument\n";
            }
            my ( $sender, $classname, $secondargument, $methodname, $end, $after);

            my $connectArgument_regexp = qr/
                                 ^([^,]*)\s*                 # (1) sender
                                 ,\s*static_cast\s*<\s*void\s*\((.*\*\)) #(2) classname
                                 \((.*)\)\> #(2) secondargument
                                 (\(&.*\)) #methodname
                                 #,\s*static_cast\s*<\s*void\s*(\(\s*\*\))
                                 ,\s*(.*)$                       # (6) after
                                 /x;
                                 
            if (($sender, $classname, $secondargument, $methodname, $after) = $argument =~ $connectArgument_regexp)
            {
                #warn "SPLIT!!!!!!!! : SENDER: $sender, CLASSNAME : $classname, SECONDARGUMENT: $secondargument, METHODENAME: $methodname, $end, $after\n";
                $_ = $indent . "connect" . $sender . ", QOverload<$secondargument>::of$methodname,$end $after;\n";
            } else {
               if (defined $toorig) {
                  $_ = $toorig;
                  undef $toorig;
               }
            }
            undef $toorig;
        }
        else {
            if (/^(\s*(?:[\-\>:\w]+)?)\bconnect\b\s*/) {
                warn "It's perhaps a multi line " . $_ . "\n";
                $tojoin = $_;
                $toorig = $_;
                $_      = "";
            }
        }
        $currentLine++;
        $modified ||= $orig ne $_;
        $_;
    } <$FILE>;

    if ($modified) {
        open( my $OUT, ">", $file );
        print $OUT @l;
        close($OUT);
    }
}
functionUtilkde::diffFile("@ARGV");
