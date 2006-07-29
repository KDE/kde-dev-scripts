## add_trace.pl
## Script to add a kdDebug() call as the first line of each method
## including as many parameters as possible (i.e. those supported by kdDebug)
## Very useful for tracing.
##
## Usage: perl -i add_trace.pl myfile.cpp
##
## Generates all statement with kdDebug(0) so that it is very easy
## to remove them afterwards :
## perl -pi -e 'if (/kdDebug\(0\)/) { $_ = ""; }' myfile.cpp
## perl -pi -e 'if (/#include <kdebug.h> \/\/ inserted by add_trace/) { $_ = ""; }' myfile.cpp
## 
## needs perl 5.8+ for the Tie::File module
##
## Written by David Faure <faure@kde.org>, licensed under pizzaware.
## 18/03/2000

use Tie::File;

sub insert_kdebug()
{
# inserts a line "include <kdebug.h>" at the beginning of each file
  for ($i=0;$i<$#ARGV;$i++)
  {
    tie @LOG, 'Tie::File', $ARGV[$i];
    unshift @LOG, '#include <kdebug.h> // inserted by add_trace';
  }
}

insert_kdebug();
$insignature=0;
while (<>)
{
  if ( $insignature )
    {
      $statement .= $_;
      chop;
      $oneline .= $_;
    }
  elsif ( /^[^\s]+\s*[^\s]+::[^\s]+/ && !/typedef\s/ && !/^\s*class\s/ && !/^[^\s]+\s*[^\s]+::[^\s]+.*;/ && !/ *\/\// )
  # A function declaration starts. A function is not a typedef, not a class, not a command and not a comment.
    {
      $insignature = 1;
      $statement = $_;
      chop;
      $oneline = $_;
    }
  elsif ( /^\/\/.*/)
    {
      # comment
      # do nothing
      $insignature = 0;
    }
  # [^\s]+ means, one ore more characters that are no spaces
  elsif ( /^[^\s]+\s*[^\s]+::[^\s]+.*\}/ && !/typedef\s/ && !/^\s*class\s/ )
    {
      # declaration and implementation in one line
      # do nothing
      $insignature = 0;
    }
  if ( $insignature )
    {
      if ( /\{/ ) # End of signature
	{
	  $insignature = 0;
	  $_ = $oneline;
	  #print STDERR "Signature : $_\n";
	  print $statement;
	  $line = "  kdDebug(0)";
	  if ( m/([^\*\s]+::[^\s]+)\(/ )
	    {
	      $line = $line . " << \"$1\"";
	    }
	  ## Ok now extract args
	  s/\/\*.*\*\///;
	  s/\s*\/\/.*//; # remove comments
	  s/^.*\([\s]*//; # Remove everything before first '('
	  s/\s*\)\s*:\s+.*$/,/; # Remove any ") : blah", replace with a ','
	  s/\s*\).*\{\s*$/,/; # Remove anything after ')', replace with a ','
          s/ const[&] / /g;
	  #print STDERR "Args list : $_\n";
	  @args = split( ",", $_ );
	  foreach (@args)
	    {
	      s/^\s*//;
	      s/\s*$//;
	      #print STDERR "Argument: $_\n";
	      ## Pointer ?
	      if ( m/[a-zA-Z0-9_\s]+\*\s*([a-zA-Z0-9_]+)/ ) {
		$line = $line . " << \" $1=\" << " . $1;
	      }
	      ## int, long ?
	      elsif ( m/^int\s+([a-zA-Z0-9_]+)/ || m/^long\s*([a-zA-Z0-9_]+)/ ) {
		$line = $line . " << \" $1=\" << " . $1;
	      }
	      ## bool
	      elsif ( m/^bool\s+([a-zA-Z0-9_]+)/ ) {
		$line = $line . " << \" $1=\" << (" . $1 . " ? \"true\" : \"false\" )";
	      }
	      ## QString and friends
	      elsif ( m/QString[\&\s]+([a-zA-Z0-9_]+)/ || m/QCString[\&\s]*([a-zA-Z0-9_]+)/ ) {
		$line = $line . " << \" $1=\" << " . $1;
	      }
	      ## KURL
	      elsif ( m/KURL[\&\s]+([a-zA-Z0-9_]+)/ ) {
		$line = $line . " << \" $1=\" << " . $1 . ".url()";
	      }
	    }
	  $line = $line . " << endl;\n";
	  if ( !m/\}/ ) {print $line;}
	  #print STDERR "Debug call added : $line\n";
	}
    }
  else
    {
        # Normal line
        print;
    }
}
if ( $insignature )
{
   print STDERR "Warning, unterminated method signature !! Check the file !\n";
   print $statement;
}
