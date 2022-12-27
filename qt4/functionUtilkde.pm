# Util function for script to convert kde3 to kde4
# laurent Montel <montel@kde.org> 2005-2006-2007-2008-2009 GPL

package functionUtilkde;
use strict;
sub diffFile
{
	if (-d ".svn") {
		system(qw(svn diff), @_);
	} elsif (-d "CVS") {
		system(qw(cvs diff -up), @_);
	} elsif (-d ".git") {
                system(qw(git diff), @_);
	}
	warn "files to commit: @_\n";
}

sub selectHeaderFile
{
    my ($newFile) = @_;
    return 1 if -d $newFile;
    if ( $newFile =~ /(\.h$)/ ) {
       return 0;
    }
    return 1;
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

sub addAfterAllIncludes($$)
{
    local *F;
    my ( $file, $theline ) = @_;
    open F, "+<", $file or do { print STDOUT "open($file) failed : \"$!\"\n"; next };
    my $str = join '', <F>;
    if ( $str !~ /$theline/ ) {

        # Add the include after all others
        if ( !( $str =~ s!(#include <.*#include <[^\r\n]*)!$1\n$theline!smig ) ) {

            # This failed, maybe there is only one include
            if ( ! ($str =~ s!(#include <[^\r\n]*)!$1\n$theline!smig ) ) {
                warn "Couldn't add $theline\n in $file\n";
            }
        }
        seek F, 0, 0;
        print F $str;
        truncate F, tell(F);
    }
    close F;
}

sub addIncludeInFile($$)
{
    my ( $file, $includefile ) = @_;
    addAfterAllIncludes($file, "#include <$includefile>");
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
