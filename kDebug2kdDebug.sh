## kDebug2kDebug.sh
## Script to port from qDebug, kdebug, kDebugInfo etc. to kDebug/kWarning/...
## Example:
## kDebugInfo( [area,] "format %a - %b", arga, argb )
## becomes
## kDebug( [area] ) << "format " << arga << " - " << argb << endl;
##
## Written by David Faure <faure@kde.org>, licensed under GPL.
## 17/03/2000

find $1 -name '*[cCph]' -type f | xargs grep -H -i 'ebug(\|warning(' \
| grep -v 'kDebug\|kWarning' \
| grep -v include \
| sed -e "s#:.*##" \
| sort -u \
| while read file; do
echo -n "working on $file "
cp $file $file.tmp
perl -w -i -e \
'
$inkdebug=0;
while (<>)
{
    if ( $inkdebug )
    {
	chop;
	#print "Reading line : " . $_ . "\n";
	$statement .= $_;
    }
    elsif ( /kdebug\s*\(/ || /kDebug[a-zA-Z]*\s*\(/ || /qDebug\s*/ || /qWarning\s*/ )
    {
	# Very old kdebug stuff :)
	s/kdebug\s*\(\s*KDEBUG_INFO,/kDebugInfo\(/;
	s/kdebug\s*\(\s*0,/kDebugInfo\(/;
	s/kdebug\s*\(\s*KDEBUG_WARN,/kDebugWarning\(/;
	s/kdebug\s*\(\s*KDEBUG_ERROR,/kDebugError\(/;
	s/kdebug\s*\(\s*KDEBUG_FATAL,/kDebugFatal\(/;

	$inkdebug = 1;
	chop;
	$statement = $_;
    }

    if ( $inkdebug )
    {
	if ( /\)\s*;/ ) # look for );
	{
	    $inkdebug = 0;
	    $_ = $statement;
	    ## Ok, now we have the full line
	    ## 1 - Parse
	    if (s/(^.*kDebug[a-zA-Z]*)\s*\(\s*//) {
	      $line=$1; # has the indentation, //, and the kDebug* name
            } elsif (s/(^.*qDebug)\s*\(\s*// || s/(^.*qWarning)\s*\(\s*//) {
              $line=$1;
            } else { die "parse error on kDebug/qDebug/qWarning..."; }
	    $line=$1; # has the indentation, //, and the kDebug* name
	    $line =~ s/kDebugInfo/kDebug/;
	    $line =~ s/kDebugArea/kDebug/;
	    $line =~ s/qDebug/kDebug/;
	    $line =~ s/qWarning/kWarning/;
	    $line =~ s/kDebugWarning/kWarning/;
	    $line =~ s/kDebugError/kError/;
	    $line =~ s/kDebugFatal/kFatal/;
	    $area = "";
	    if ( s/^([0-9]+)\s*,\s*//) # There is an area
	    {
		$area = $1;     # Store it
		$line .= "(" . $area . ")";
	    } elsif ( s/^(KBABEL[^,]*)\s*,\s*//)
	    {     # Example of support for #defined area (here KBABEL.*)
		$area = $1;     # Store it
		$line .= "(" . $area . ")";
	    } else
	    { $line .= "()";  }  # You can set an area here if converting qDebugs

            $arguments = ""; # for final test
            $commented = 0;
	    if ( !s/^\"([^\"]*)\"// ) # There is no format
	    {
		s/\s*\)\s*;\s*$//;
		$commented = s/\s*\)\s*;\s*\*\/$//; # terminating with */
		$line = $line . " << " . $_ ;
	    } else
	    {
		$format = $1;
                # If we stopped on a \" we need to keep adding to format
                while ( $format =~ m/\\$/ )
                    { s/^([^\"]*)\"// || die "problem"; $format .= "\"" . $1; }
		s/\s*\)\s*;\s*$/,/; # replace trailing junk with , for what follows
		$commented = s/\s*\)\s*;\s*\*\/$/,/; # terminating with */
		$arguments = $_;

		## 2 - Look for %x
		@stringbits = split( "(%[0-9]*[a-z])", $format );
		foreach ( @stringbits )
		{
		    #print $_ . "\n";
		    if ( /(%[0-9]*[a-z])/ ) # This item is a format
		    {
			## 3 - Find argument
			# kludge for QString(a,b) constructions
			$arguments =~ s/(QString\s*\([^,]+,[^,]+\))/QStrKLUDGE/;
			$kludge = $1;
			$arguments =~ s/\s*([^,]+)\s*,//;
			# Remove trailing .ascii() and latin1()
			$arg = $1;
			$arg =~ s/QStrKLUDGE/$kludge/; ## restore original arg
			$arg =~ s/\.ascii\(\)$//;              # remove
			$arg =~ s/\.latin1\(\)$//;             # remove
			$arg =~ s/debugString\(([^\)]+)\)/$1/; # remove
                        # If "a ? b : c" then add parenthesis
                        if ( $arg =~ m/.+\s*\?\s*.+\s*:\s*.+/ ) {
                           $arg = "(" . $arg . ")";
                        }
			$line = $line . " << " . $arg;
		    } else # This item is some litteral
		    {
			$line = $line . " << \"" . $_ . "\"" if ($_);
		    }
		}
		
	    }
            $arguments =~ s/,$//; # Remove trailing slash before next check
            if ( $arguments ) {
               print STDERR "Non-processed (Information lost! Check the file!) : " . $arguments . "\n";
            }
	    $line = $line . " << endl;\n";
            if ( $commented ) { $line .= "\*/"; }
	    print $line;
	}
    }
    else
    {
        # Normal line
	print;
    }
}
if ( $inkdebug )
{
   print STDERR "Warning, unterminated kDebug call !! Check the file !\n";
   print $arguments;
}
' $file.tmp
if cmp -s $file $file.tmp > /dev/null 2>&1 ; then
  echo "unchanged"
  rm $file.tmp  
else
  echo "patching"
  mv $file.tmp $file
fi

done

