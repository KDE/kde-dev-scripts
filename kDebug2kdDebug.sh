## kDebug2kdDebug.sh
## Script to port from kDebugInfo and friends to kdDebug and friends.
## Example:
## kDebugInfo( [area,] "format %a - %b", arga, argb )
## becomes
## kdDebug( [area] ) << "format " << arga << " - " << argb << endl;
##
## Written by David Faure <faure@kde.org>, licensed under GPL.
## 17/03/2000

find $1 -name '*[cCph]' -type f | while read file; do
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
    elsif ( /kDebug[a-zA-Z]*\s*\(/ || /qDebug\s*/ )
    {
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
            } elsif (s/(^.*qDebug)\s*\(\s*//) {
              $line=$1;
            } else { die "parse error on kDebug/qDebug..."; }
	    $line=$1; # has the indentation, //, and the kDebug* name
	    $line =~ s/kDebugInfo/kdDebug/;
	    $line =~ s/qDebug/kdDebug/;
	    $line =~ s/kDebugWarning/kdWarning/;
	    $line =~ s/kDebugError/kdError/;
	    $line =~ s/kDebugFatal/kdFatal/;
	    $area = "";
	    if ( s/^([0-9]+)\s*,\s*//) # There is an area
	    {
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
			$arg =~ s/\.ascii\(\)$//;
			$arg =~ s/\.latin1\(\)$//;
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
               print STDERR "Non-processed : " . $arguments . "\n";
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
' $file

done

