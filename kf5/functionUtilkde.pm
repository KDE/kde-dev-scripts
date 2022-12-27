# Util function for script to convert kde3 to kde4
# laurent Montel <montel@kde.org> 2005-2006-2007-2008-2009 GPL

package functionUtilkde;
use strict;

our $paren_begin = '(\((?:(?>[^()]+)|(?';
our $paren_end = '))*\))';

sub extraVariableFromUiFile($$)
{
    my ($ref_localvarname, $ref_uiclassname) = @_;
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
           warn "Found class in ui file \'$uifile\', className: \'$className\', variable: \'$variableName\'\n";
           ${$ref_localvarname}{$variableName} = ${className};
        }
        if (/\<class\>(.*)\<\/class\>/) {
           if (not defined $mainClassFound) {
             my $name = $1;
             ${$ref_uiclassname}{$name} = 1;
             warn "Found Class Name in file \'$uifile\': name \'$name\'\n";
             $mainClassFound = 1;
           }
        }
        # Special case buttongroup
        if ( /\<buttongroup name=\"(.*)\"\/\>/ ) {
           my $className = "QButtonGroup";
           my $variableName = $1;
           ${$ref_localvarname}{$variableName} = ${className};
           warn "Found QButtonGroup in file \'$uifile\': name \'$variableName\'\n";
        }

        $_;
      } <$FILE>
    }
}

sub diffFile
{
    if ( system("git rev-parse --is-inside-work-tree 2>/dev/null >/dev/null") == 0 ) {
        system( qw(git diff --), @_ );
    } elsif ( system("svn info 2>/dev/null >/dev/null") == 0 ) {
        system( qw(svn diff), @_ );
    } elsif ( system("hg identify 2>/dev/null >/dev/null") == 0 ) {
        system( qw(hg diff), @_ );
    } elsif ( -d "CVS" ) {
        system( qw(cvs diff -up), @_ );
    }
}

sub headerName
{
    my ($cppfile) = @_;
    my $headerfile;
    if ( $cppfile =~ /.cpp$/ ) {
       $cppfile =~ s/.cpp$//;
       $headerfile = $cppfile . ".h";
       unless ( -e $headerfile ) {
          $headerfile = $cppfile . "_p.h";
       }
    } elsif ( $cppfile =~ /.cc$/ ) {
       $cppfile =~ s/.cc$//;
       $headerfile = $cppfile . ".h";
       unless ( -e $headerfile ) {
          $headerfile = $cppfile . "_p.h";
       }
    }
    return $headerfile;
}

sub excludeFile
{
    my ($newFile) = @_;
    return 1 if -d $newFile;
    if ( $newFile =~ /(\.cpp$|\.cc$|\.h$|\.ui$|\.CC$|\.c\+\+$)/ ) {
       return 0; 
    }
    return 1;
    #return 1 if $newFile =~ /\/\.svn\//;
    #return $newFile =~ /TODO|Changelog|ChangeLog|README|Readme|portinglog.txt|Makefile(|\.(in|am))|\.(jpg|png|svgz|svg|html|HOWTO|README|svn|ui|kidl|desktop|pl|libs|o|moc|docbook|gz|ogg|sh|wav|cmake|dat|au|dox|l[ao])?$/;
}

sub removeObjectNameTwoArgument
{
    my ($newLine, $className) = @_;
    my $result;
    if ( $newLine =~ /$className\s*\(/ ) {
	if (my ($blank, $before, $prefix, $contenu) = $newLine =~ m!^(\s*)(\s*.*)(new $className.*?)\(([^()]*)\s*\);$!) {
	    if ( my ($firstelement, $secondelement) = m!.*?\(\s*([^,]*),\s*(\"[^\"]*\")\s*\);\s*$!) {
                my $split = $before;
                $split =~ s!$className!!;
                $split =~ s!=!!;
                $split =~ s! !!g;
				$split =~ s!\*!!;
		
		# normalize white space around the arguments in caller:
		$secondelement =~ s/\s*$//;
		$secondelement =~ s/^\s*//;
		
		# do the actual conversion:
                $result = $blank . $before . "$prefix\( $firstelement \);\n" . $blank . $split . "->setObjectName\( $secondelement \);\n";
            }
        }
    }
    return $result;
}

sub removeObjectNameThreeArgument
{
    my ($newLine, $className) = @_;
    my $result;
    if ( $newLine =~ /$className\s*\(/ ) {
	if (my ($blank, $before, $prefix, $contenu) = $newLine =~ m!^(\s*)(\s*.*)(new $className.*?)\(([^()]*)\s*\);$!) {
	    if ( my ($firstelement, $secondelement, $thirdelement) = m!.*?\(\s*([^,]*),\s*([^,]*),\s*(\"[^\"]*\")\s*\);\s*$!) {
				my $split = $before;
                $split =~ s!$className!!;
                $split =~ s!=!!;
                $split =~ s! !!g;
				$split =~ s!\*!!;
		
		# normalize white space around the arguments in caller:
		$thirdelement =~ s/\s*$//;
		$thirdelement =~ s/^\s*//;
		# do the actual conversion:
                $result = $blank . $before . "$prefix\( $firstelement, $secondelement \);\n" . $blank . $split . "->setObjectName\( $thirdelement \);\n";
            }
        }
    }
    return $result;
}

sub addIncludeInFile
{
   local *F;
   my ($file, $includefile) = @_;
   open F, "+<", $file or do { print STDOUT "open($file) failed : \"$!\"\n"; next };
   my $str = join '', <F>;
   if( $str !~ /#include <$includefile>/ ) {
    # Add the include after all others
    if (!($str =~ s!(#include <.*#include <[^\n]*)!$1\n#include <$includefile>!smig)) {
       # This failed, maybe there is only one include
       $str =~ s!(#include <[^\n]*)!$1\n#include <$includefile>!smig;
    }
    seek F, 0, 0;
    print F $str;
    truncate F, tell(F);
   }
   close F;
}

sub removeIncludeInFile
{
   local *F;
   my ($file, $includefile) = @_;
   open F, "+<", $file or do { print STDOUT "open($file) failed : \"$!\"\n"; next };
   my $str = join '', <F>;
   if( $str =~ m/#include <$includefile>/ ) {
     $str =~ s!#include <$includefile>!!smig;
     seek F, 0, 0;
     print F $str;
     truncate F, tell(F);
   }
   close F;
}


sub cleanSpace
{
  my ($var) = @_;
  $var =~ s/ //g;
  return $var;
}

# code from MDK::common package
# Code from Drakx Mandriva code (GPL code)
sub substInFile(&@) {
    my ($f, $file) = @_;
    my $linkdest;
    #- try hard to keep symlinks as they were set
    if (-l $file) {
        my $targetfile = readlink $file;
        unlink $file;
        $linkdest = $file;
        $file = $targetfile;
    }
    my $modified = 0;
    if (-s $file) {
	local @ARGV = $file;
	local $^I = '';
	local $_;
	while (<>) {
	    $_ .= "\n" if eof && !/\n/; # always terminate with a newline
	    my $orig = $_;
	    &$f($_);
	    $modified ||= $orig ne $_;
	    print;
	}
    } else {
	local *F; my $old = select F; # that way eof return true
	local $_ = '';
	&$f($_);
	select $old;
	eval { output($file, $_) };
    }
    $linkdest and symlink $file, $linkdest;
    return $modified;
}
1;
