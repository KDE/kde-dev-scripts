#!/usr/bin/perl

# laurent Montel <montel@kde.org> 2006 GPL
# This function allows to adapt file to new kconfig  API
# in KDE4 writePathEntry/deleteEntry/deleteGroup API changed
# Now we used QFLAGS
# 
use File::Basename;
use lib dirname( $0 );
use functionUtilkde;
use strict;

open(my $F, q(find -name "*" |));
my $file;
while ($file = <$F>) {
    chomp $file;
    next if functionUtilkde::excludeFile( $file);

    my $modified;
    my @necessaryIncludes = ();
    open(my $FILE, $file) or warn "We can't open file $file:$!\n";
    my @l = map {
	my $orig = $_;

	# writePathEntry change API
	if (my ($prefix, $parenthese, $end) = /(.*writePathEntry\s*\()(.*)(\).*)/) {
			#warn "prefix !!!! :<$prefix>  parenthese <$parenthese> end <$end>\n";	
	    my $changes = $end;
	    $changes =~ s/\)//;
	    if ( my ($firstelement, $secondelement,$thirdelement,$fouthelement,$fifthelement) = $parenthese =~ m!(.*),\s*(.*),\s*(.*),\s*(.*),\*(.*)$!) {
				#warn "5 elements : firstelement <$firstelement>  secondelement <$secondelement> thirdelement<$thirdelement> fouthelement <$fouthelement> fifthelement <fifthelement> \n";
		if ( $thirdelement =~ /(true|TRUE)/ && $fouthelement =~ /(true|TRUE)/ && $fifthelement =~ /(true|TRUE)/  )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Normal|KConfigBase::Global|KConfigBase::NLS" .  $end . "\n";
	        }
		elsif ( $thirdelement =~ /(false|FALSE)/ && $fouthelement =~ /(true|TRUE)/ && $fifthelement =~ /(true|TRUE)/  )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Global|KConfigBase::NLS" .  $end . "\n";
		}
		elsif ( $thirdelement =~ /(true|TRUE)/ && $fouthelement =~ /(false|FALSE)/ && $fifthelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Normal|KConfigBase::NLS" .  $end . "\n";
		}
		
	    }
	    elsif ( my ($firstelement, $secondelement,$thirdelement,$fouthelement) = $parenthese =~ m!(.*),\s*(.*),\s*(.*),\s*(.*)$!) {
		#warn "4 elements : firstelement <$firstelement>  secondelement <$secondelement> thirdelement<$thirdelement> fouthelement <$fouthelement> \n";
		if ( $thirdelement =~ /(true|TRUE)/ && $fouthelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Normal|KConfigBase::Global" .  $end . "\n";
	        }
		elsif ( $thirdelement =~ /(false|FALSE)/ && $fouthelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Global" .  $end . "\n";
		}
		elsif ( $thirdelement =~ /(true|TRUE)/ && $fouthelement =~ /(false|FALSE)/ )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Normal" .  $end . "\n";
		}
		
	    }
	    elsif ( my ($firstelement, $secondelement,$thirdelement) = $parenthese =~ m!(.*),\s*(.*),\s*(.*)$!) {
				#warn " 3 elements :  firstelement <$firstelement>  secondelement <$secondelement> thirdelement<$thirdelement>\n";
		if ( $thirdelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Normal" .  $end . "\n";
		}
	    }
	}	

	# deleteGroup Change API
	if (my ($prefix, $parenthese, $end) = /(.*deleteGroup\s*\()(.*)(\).*)/) {
	#warn "prefix !!!! :<$prefix>  parenthese <$parenthese> end <$end>\n";	
	    my $changes = $end;
	    $changes =~ s/\)//;
	    if ( my ($firstelement, $secondelement,$thirdelement) = $parenthese =~ m!(.*),\s*(.*),\s*(.*)$!) {
		#warn " 3 elements :  firstelement <$firstelement>  secondelement <$secondelement> thirdelement<$thirdelement>\n";
		if ( $secondelement =~/(false|FALSE)/ && $thirdelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, KConfigBase::Recursive|KConfigBase::Global" .  $end . "\n";
		}
		elsif ( $secondelement =~/(true|TRUE)/ && $thirdelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, KConfigBase::Global" .  $end . "\n";
		}
	    }
	}	
	
	# deleteEntry change API
	if (my ($prefix, $parenthese, $end) = /(.*deleteEntry\s*\()(.*)(\).*)/) {
	    warn "prefix !!!! :<$prefix>  parenthese <$parenthese> end <$end>\n";	
	    my $changes = $end;
	    $changes =~ s/\)//;
	    if ( my ($firstelement, $secondelement,$thirdelement) = $parenthese =~ m!(.*),\s*(.*),\s*(.*)$!) {
				#warn " 3 elements :  firstelement <$firstelement>  secondelement <$secondelement> thirdelement<$thirdelement>\n";
		if ( $secondelement =~/(false|FALSE)/ && $thirdelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, KConfigBase::Global" .  $end . "\n";
		}
		elsif ( $secondelement =~/(true|TRUE)/ && $thirdelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, KConfigBase::NLS|KConfigBase::Global" .  $end . "\n";
		}
		elsif ( $secondelement =~/(true|TRUE)/ && $thirdelement =~ /(false|FALSE)/ )
		{
		    $_ = $prefix . "$firstelement, KConfigBase::NLS" .  $end . "\n";
		}
	    }
	}	

	# writeEntry change API
	if (my ($prefix, $parenthese, $end) = /(.*writeEntry\s*\()(.*)(\).*)/) {
	#warn "prefix !!!! :<$prefix>  parenthese <$parenthese> end <$end>\n";	
	    my $changes = $end;
	    $changes =~ s/\)//;
	    if ( my ($firstelement, $secondelement,$thirdelement,$fouthelement) = $parenthese =~ m!(.*),\s*(.*),\s*(.*),\s*(.*)$!) {
		#warn "4 elements : firstelement <$firstelement>  secondelement <$secondelement> thirdelement<$thirdelement> fouthelement <$fouthelement> \n";
		if ( $thirdelement =~ /(true|TRUE)/ && $fouthelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Normal|KConfigBase::Global" .  $end . "\n";
	        }
		elsif ( $thirdelement =~ /(false|FALSE)/ && $fouthelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Global" .  $end . "\n";
		}
		elsif ( $thirdelement =~ /(true|TRUE)/ && $fouthelement =~ /(false|FALSE)/ )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Normal" .  $end . "\n";
		}
		
	    }
	    elsif ( my ($firstelement, $secondelement,$thirdelement) = $parenthese =~ m!(.*),\s*(.*),\s*(.*)$!) {
				#warn " 3 elements :  firstelement <$firstelement>  secondelement <$secondelement> thirdelement<$thirdelement>\n";
		if ( $thirdelement =~ /(true|TRUE)/ )
		{
		    $_ = $prefix . "$firstelement, $secondelement, KConfigBase::Normal" .  $end . "\n";
		}
	    }
	}	
	$modified ||= $orig ne $_;
	$_;
    } <$FILE>;

    if ($modified) {
	open (my $OUT, ">$file");
	print $OUT @l;
    }

}
functionUtilkde::diffFile( <$F> );

