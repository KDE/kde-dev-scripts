#!/usr/bin/perl -w

# Laurent Montel <montel@kde.org> (2014)
# KProgressDialog->QProgressDialog
# find -iname "*.cpp" -o -iname "*.h" |xargs kde-dev-scripts/kf5/convert-kprogressdialog.pl
# TODO need to improve it.
use strict;
use File::Basename;
use lib dirname($0);
use functionUtilkde;

foreach my $file (@ARGV) {

    my $modified;
    open(my $FILE, "<", $file) or warn "We can't open file $file:$!\n";
    my %varname = ();
    my @l = map {
        my $orig = $_;
        my $regexp = qr/
           ^(\s*)                             # (1) Indentation, possibly "Classname *" (the ? means non-greedy)
           (.*?)                              # (2) Possibly "Classname *" (the ? means non-greedy)
           (\w+)                              # (3) variable name
           \s*=\s*                            #   assignment
           new\s+KProgressDialog\s*\((.*)\)   # (4)  new KProgressDialog(...,...,...,...);
           (.*)$                              # (5) afterreg
           /x; # /x Enables extended whitespace mode
        if (my ($indent, $left, $var, $args, $afterreg) = $_ =~ $regexp) {
            warn "Found KProgressDialog $var \n";
            $varname{$var} = 1;
            my $constructor_regexp = qr/
                                 ^([^,]*)\s*        # parent
                                 (?:,\s([^,]*))?    # caption
                                 (?:,\s([^,]*))?    # text
                                 (.*)$              # after
                                 /x;
            if ( my ($parent, $caption, $text,  $after) = $args =~ $constructor_regexp ) {
                $_ = $indent . $left . "$var = new QProgressDialog($parent);\n";
                if (defined $caption) {
                   $_ .= $indent . "$var\-\>setWindowTitle($caption);\n";
                }
                if (defined $text) {
                   $_ .= $indent . "$var\-\>setLabelText($text);\n";
                }
            }
        }
        my $regexpLocal = qr/
          ^(\s*)           # (1) Indentation
          KProgressDialog\s+
          (\w+)            # (2) variable name
          (.*)/x; # /x Enables extended whitespace mode
        if (my ($left, $var, $args) = $_ =~ $regexpLocal) {
           $varname{$var} = 1;
            my $constructor_regexp = qr/
                                 ^([^,]*)\s*        # parent
                                 (?:,\s([^,]*))?    # caption
                                 (?:,\s([^,]*))?    # text
                                 (.*)$              # after
                                 /x;
            if ( my ($parent, $caption, $text,  $after) = $args =~ $constructor_regexp ) {
                $_ = $left . "QProgressDialog $var$parent);\n";
                if (defined $caption) {
                   $_ .= $left . "$var\.setWindowTitle($caption);\n";
                }
                if (defined $text) {
                   $_ .= $left . "$var\.setLabelText($text\n";
                }
            }
        }
        if (/(\w+)\.progressBar\(\)\-\>setMaximum\b/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
                s/$variable\.progressBar\(\)\-\>setMaximum\b/$variable\.setMaximum/;
            }
        }
        if (/(\w+)\.progressBar\(\)\-\>setMinimum\b/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
                s/$variable\-\>progressBar\(\)\.setMinimum\b/$variable\.setMinimum/;
            }
        }
        if (/(\w+)\.progressBar\(\)\-\>setValue\b/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
                s/$variable\.progressBar\(\)\-\>setValue\b/$variable\.setValue/;
            }
        }
        if (/(\w+)\.progressBar\(\)\-\>value\b/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
                s/$variable\.progressBar\(\)\-\>value\b/$variable\.value/;
            }
        }
        if (/(\w+)\.progressBar\(\)\-\>setRange\b/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
                s/$variable\.progressBar\(\)\-\>setRange\b/$variable\.setRange/;
            }
        }
        if (/(\w+)\.wasCancelled\s*\(\)/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
               s/$variable\.wasCancelled\s*\(\)/$variable\.wasCanceled()/;
            }
        }
        if (/(\w+)\.setAllowCancel\s*\(\s*false\s*\);/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
               s/$variable\.setAllowCancel\s*\(\s*false\s*\);/$variable\.setCancelButton\(0\);/;
            }
        }
        if (/(\w+)\.setAllowCancel\s*\(\s*true\s*\);/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
               $_ = "";
            }
        }
        if (/(\w+)\.setShowCancel\s*\(\s*false\s*\);/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
               s/$variable\.setShowCancel\s*\(\s*false\s*\);/$variable\.setCancelButton\(0\);/;
            }
        }

        if (/(\w+)\-\>setShowCancel\s*\(\s*false\s*\);/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
               s/$variable\-\>setShowCancel\s*\(\s*false\s*\);/$variable\-\>setCancelButton\(0\);/;
            }
        }


        if (/(\w+)\-\>progressBar\(\)\-\>setMaximum\b/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
                s/$variable\-\>progressBar\(\)\-\>setMaximum\b/$variable\-\>setMaximum/;
            }
        }
        if (/(\w+)\-\>progressBar\(\)\-\>setMinimum\b/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
                s/$variable\-\>progressBar\(\)\-\>setMinimum\b/$variable\-\>setMinimum/;
            }
        }
        if (/(\w+)\-\>progressBar\(\)\-\>setValue\b/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
                s/$variable\-\>progressBar\(\)\-\>setValue\b/$variable\-\>setValue/;
            }
        }
        if (/(\w+)\-\>progressBar\(\)\-\>value\b/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
                s/$variable\-\>progressBar\(\)\-\>value\b/$variable\-\>value/;
            }
        }
        if (/(\w+)\-\>progressBar\(\)\-\>setRange\b/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
                s/$variable\-\>progressBar\(\)\-\>setRange\b/$variable\-\>setRange/;
            }
        }
        if (/(\w+)\-\>wasCancelled\s*\(\)/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
               s/$variable\-\>wasCancelled\s*\(\)/$variable\-\>wasCanceled()/;
            }
        }
        if (/(\w+)\-\>setAllowCancel\s*\(\s*false\s*\);/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
               s/$variable\-\>setAllowCancel\s*\(\s*false\s*\);/$variable\-\>setCancelButton\(0\);/;
            }
        }
        if (/(\w+)\-\>setAllowCancel\s*\(\s*true\s*\);/) {
            my $variable = $1;
            if (defined $varname{$variable}) {
               $_ = "";
            }
        }
        if ( /\s*connect\s*\(\s*(\w+),\s*SIGNAL\s*\(\s*cancelClicked\s*\(\s*\)/ ) {
           if (defined $varname{$1} ) {
              s/cancelClicked/canceled/;
           }
        }

        s/\bKProgressDialog\b/QProgressDialog/g;
        s/\<KProgressDialog\b\>/\<QProgressDialog>/ if (/#include/);
        s/\<kprogressdialog.h\>/\<QProgressDialog>/ if (/#include/);
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
