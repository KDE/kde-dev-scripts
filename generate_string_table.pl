#/usr/bin/env perl
# Author: Thiago Macieira <thiago@kde.org>
#  This program is released into the public domain by the author.
#
# Feed this program a list of strings, one per line, and it'll generate
# a C index list. All non-relocated data.

$varname = "data";
$varname = $ARGV[0] if $ARGV[0];

print "static const char ${varname}_string[] =\n";
$counter = 0;
$i = 0;
%hash = { };
while (<STDIN>) {
    chomp;
    if (defined($hash{$_})) {
	# Entry already seen, output one of the old addresses
	print STDERR "Already seen " . $_ . "  " . $hash{$_} . "\n";
	$sizes[$i++] = $hash{$_};
	next;
    }

    m/^(i18n:)?(.*)$/i;
    print "    \"$2\\0\"\n" if (!$1);
    print "    I18N_NOOP(\"$2\")\"\\0\"\n" if ($1);
    $hash{$_} = $counter;
    $hash{""} = $counter + length $2 if ($i == 0); # make the empty string point to the first \0
    $sizes[$i++] = $counter;

    $counter += 1 + length $2;
}

print "    \"\\0\";\n\nstatic const int ${varname}_indices[] = {";
for ($j = 0; $j < $i; ++$j) {
    if (($j % 8) == 0) {
	print "\n   ";
    }

    printf " %4d,", $sizes[$j];
}
if (($j % 8) == 0) {
    print "\n   ";
}
print "   -1\n};\n";
