# Util function for script to convert kde3 to kde4
# Montel Laurent <montel@kde.org>

package functionUtilkde;
use strict;
sub diffFile
{
		my $listFileDiff = join (" ", @_);
		system("svn diff $listFileDiff");
		warn "file to commit : $listFileDiff\n";
}

sub excludeFile
{
    my ($newFile) = @_;
    return $newFile =~ /\.(svn|ui|kidl|desktop|pl|libs|o|moc|docbook|dox|l[ao])|TODO|Changelog|ChangeLog|README|Readme|Makefile(.(in|am))?$/;
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
   open F, "+<", $file or do { print STDOUT "open($file) failled : \"$!\"\n"; next };
   my $str = join '', <F>;
   if( $str !~ /#include <$includefile>/ ) {
   $str =~ s!(#include <.*#include <[^
]*)!\1\n#include <$includefile>!smig;
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
    if (-s $file) {
    local @ARGV = $file;
    local $^I = '';
    local $_;
    while (<>) {
        $_ .= "\n" if eof && !/\n/;
        &$f($_);
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
}
1;
