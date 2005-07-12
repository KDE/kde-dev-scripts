# Util function for script to convert kde3 to kde4
# Montel Laurent <montel@kde.org>

package functionUtilkde;
use strict;
sub diffFile
{
		my $listFileDiff = join (" ", @_);
		#print "la liste qui est join : $listFileDiff \n";
		system("svn diff $listFileDiff");
}

# code from MDK::common package
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
