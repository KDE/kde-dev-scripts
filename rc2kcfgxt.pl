#! /usr/bin/env perl
#
# rc2kcfgxt.pl version 4 by Adriaan de Groot
#
#  This code is released to the Public Domain.
#

#
# Usage: rc2kcfgtxt.pl < rcfile > xmlfile
#
# Reads an rcfile (say, kmailrc) and writes out an KConfigXT XML
# file that represents a reasonable guess for representing the
# rc file. No guarantees about well-formedness of the XML are made.
#

#
# rc2kcfgxt.pl only guesses types Bool, UInt, and IntList. 
# Everything else is a String. You may need to edit the various
# types. As of 4-1-2003, valid types are:
#
#	  type (String|StringList|Font|Rect|Size|Color|
#	        Point|Int|UInt|Bool|Double|DateTime|
#		Int64|UInt64|IntList|Enum|Path) #REQUIRED
#

$group="" ;
$key="";

print <<EOF;
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE kcfg SYSTEM "http://www.kde.org/standards/kcfg/1.0/kcfg.dtd">
<kcfg>
EOF

while(<>)
{
	chomp;
	next unless $_;
	if (/\[([-A-Za-z 0-9]+)\]/)
	{
		$grp = $1;
		print "  </group>\n" if ($group && (not $group =~ /^MainWindow/));
		$group=$grp;
		next if ($group =~ /^MainWindow/);
		print "  <group name=\"$group\">\n";
		next;
	}

	next if $group =~ /^MainWindow/ ;

	@l = split /=/;
	$key = shift @l;
	$value = join "=",@l;
	$cfgkeyexpr = "";

	# Escape value values that are special to XML
	$value =~ s/</&lt;/;
	$value =~ s/>/&gt;/;
	$value =~ s/"/&quot;/;

	if ($key =~ /[ -,.<>;:!\]\[|}{]/)
	{
		$cfgkeyexpr = "key=\"$key\"";
		@key_parts = split /[ -,.<>;:!\]\[|}{]/,$key;
		$key = "";
		foreach $i (@key_parts)
		{
			next unless $i;
			$i =~ /([a-zA-Z0-9_])([a-zA-Z0-9_]*)/;
			$first = $1;
			$second = $2;
			$first =~ tr/a-z/A-Z/;
			$key .= $first . $second;
		}
	}

	# Find key type
	$type="";
	$type="Bool" if ( $value =~ /^(true|false|TRUE|FALSE)$/);
	$type="UInt" if ( $value =~ /^[0-9]+$/);
	$type="IntList" if ( ( not $type ) && ( $value =~ /^[0-9,]+$/ ));
	$type="String" unless $type;

	print <<EOF;
    <entry name="$key" $cfgkeyexpr type="$type">
        <label>
        </label>
        <default>$value</default>
    </entry>
EOF
}

print "  </group>\n" if ($group && (not $group =~ /^MainWindow/));

print "\n</kcfg>\n";
