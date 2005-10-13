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
    return $newFile =~ /\.(svn|ui|kidl|desktop|pl|libs|o|moc|l[ao])|Changelog|ChangeLog|README|Readme|Makefile(.(in|am))?$/;
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
