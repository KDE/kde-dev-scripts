#! /usr/bin/perl

## This script is based on cyclo.cgi, see http://homepage2.nifty.com/sophia0/
## cyclo.cgi v0.3c (CYCLIC IMAGE) 2001.04.15 by techan

# usage: png2mng <basename> <width> <height>

$basename = $ARGV[0];

# Image Width Height
@w_h = ($ARGV[1], $ARGV[2]);

# The number of repeats for base (integer > 1)
$b_repeat = 1;

# Ticks per second
$tps = 20;

## Main

&InitCrcTable;

$r = "NG\r\n\x1a\n";
$sig_p = "\x89P".$r;
$sig_m = "\x8aM".$r;

# Check File
@png = ();
$size = 1;
for ($i=0;$size > 0;) {
	$file = $basename.sprintf('%04d.png', $i+1);
	$size = -s $file;
        if ($size > 0)
        {
		open(IN, "< $file") || &Error(1);
		binmode(IN);
		read(IN, $sig, 8);
		if ($sig ne $sig_p) { close(IN); &Error(2); }
		read(IN, $png[$i], $size-8);
		close(IN);
		$i++;
	}
}
$number = $i;
$it = $number * $b_repeat;
$ti = $it;

$|=1;
binmode(STDOUT);

# Signature
print $sig_m;

# MHDR
$data = 'MHDR'.pack("N7",
	$w_h[0],	# Width
	$w_h[1],	# Height
	$tps,		# Ticks per second
	$number,	# Layers
	$ti,		# Frames
	$ti,		# Time
	583);		# Simplicity
&OutputData;

# DEFI define objects of the number specified by $number
for ($i=0;$i<$number;$i++) {
	$data = 'DEFI'.pack("n",
		$i+1);	# Object_id
			# Do_not_show: 1 byte (unsigned integer)
			# Concrete_flag: 1 byte (unsigned integer)
			# X_location: 4 bytes (signed integer)
			# Y_location: 4 bytes (signed integer)
			# Left_cb: 4 bytes (signed integer)
			# Right_cb: 4 bytes (signed integer)
			# Top_cb: 4 bytes (signed integer)
			# Bottom_cb: 4 bytes (signed integer)
	&OutputData;
	print $png[$i];
}
undef(@png);

# LOOP
$data = 'LOOP'.pack("CNC",
	0,	# Nest_level
	$it,	# Iteration_count
	6);	# Termination_condition:
		#  1: Decoder discretion, not cacheable.
		#  2: User discretion, not cacheable.
		# *3: External signal, not cacheable.
		#  4: Deterministic, cacheable.
		#  5: Decoder discretion, cacheable.
		#  6: User discretion, cacheable.
		# *7: External signal, cacheable.
		# Iteration_min: 4 bytes(unsigned integer)
		# Iteration_max: 4 bytes (unsigned integer)
		# Signal_number: 4 bytes (unsigned integer)
		# Additional signal_number: 4 bytes (unsigned integer)
&OutputData;

# SHOW
$data = 'SHOW'.pack("nnC",
	1,		# First_image
	$number,	# Last_image
	6);		# Show_mode:
			#  0:  Make the images potentially visible and display them.
			#  1:  Make the images invisible.
			#  2:  Display those that are potentially visible.
			#  3:  Mark images "potentially visible" but do not display
			#      them.
			#  4:  Display any that are potentially visible after toggling.
			#  5:  Do not display even if potentially visible after toggling.
			#  6:  Step through the images in the given range, making the
			#      next image potentially visible and display it.  Jump to
			#      the beginning of the range when reaching the end of the
			#      range.  Perform one step for each SHOW chunk (in reverse
			#      order if last_image < first_image).
			#  7:  Make the next image in the range (cycle) potentially
			#      visible but do not display it.
&OutputData;

# ENDL
$data = "ENDL\0";	# Nest_level: 1 byte
&OutputData;

# MEND
print "\0\0\0\0MEND! \xf7\xd5";

exit(0);

sub Error
{
my $e = $_[0];

$black = "\0\0\0";
$red = "\xff\0\0";
$white = "\xff\xff\xff";

if ($e == 1) { $plte = $white.$black; }
elsif ($e == 2) { $plte = $white.$red; }
else { $plte = $red.$white; }

$plte = "PLTE".$plte;

$p = $sig_p;
$p.="\0\0\0\rIHDR";
$p.="\0\0\0\x1e\0\0\0\x0c\x01\x03\0\0\0";
$p.="\x4f\xe0\x9f\x71";
$p.="\0\0\0\x06".$plte.&CalcCrc($plte);
$p.="\0\0\0\x2eIDAT";
$p.="\x78\x9c\x63\x60\x40\x05\xbf\xcf\xcb\x7c\x60\x68\xd2\x58\xd4\x01";
$p.="\x21\x3e\x81\x88\xe6\xf3\x4a\x40\xb1\x2e\xa5\x05\x0c\x4d\x9e\x4a";
$p.="\x13\x18\x7e\x69\xcc\xe9\0\xab\x05\0\xb0\x88\x10\xb8";
$p.="\x57\x3a\0\xa1";
$p.="\0\0\0\0IEND\xaeB`\x82";

$|=1;
# print "Content-type: $mime\n";
# print "Content-length: 121\n\n";
binmode(STDOUT);
print $p;
exit(1);
}

sub InitCrcTable
{
my $d;
@crc_table = ();
for (0 .. 255) {
	$d = $_;
	for (0 .. 7) {
		if ($d & 1) { $d = 0xedb88320 ^ (($d >> 1) & 0x7fffffff); }
		else { $d = ($d >> 1) & 0x7fffffff; }
	}
	$crc_table[$_] = $d;
}
}

sub CalcCrc
{
my $data = $_[0];
my $c = 0xffffffff;
foreach (unpack("C*", $data)) {
	$c = $crc_table[($c ^ $_) & 0xff] ^ (($c >> 8) & 0xffffff);
}
return(pack("N", ~$c));
}

sub OutputData
{
print pack("N", length($data)-4).$data.&CalcCrc($data);
undef($data);
}
__END__
