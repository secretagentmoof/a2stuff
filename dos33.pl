#!/usr/pkg/bin/perl
#$Id: dos.pl,v 1.4 2017/05/09 13:27:19 dogcow Exp dogcow $
# This work is licensed under a Creative Commons Attribution 4.0 International License.

use Carp;
use Getopt::Std;
use MIME::Base64;
use strict;

my (%opts, $img, $printcat);
getopts('3a:cdht:v', \%opts); # coalesce
if (defined $opts{h}) {
    &help;
}
my $dskimg = shift @ARGV or &help;

my $sectrk = (defined $opts{3} ? 13 : 16);
my $fn = shift @ARGV;
$printcat = ($opts{c} || !defined $fn);
$printcat = 0 if (defined $opts{a} || $opts{d});
my @ftypes = qw/T I A B S R a b/;

open my $dfh, '<', $dskimg or die "Couldn't open $dskimg";
$/ = undef;
binmode($dfh);
$img = <$dfh>;
close $dfh;
# if (length($img) % 4096) { die "Malformed disk size; 13 sector?" }
my $sec = 0;
my @sectors = unpack('(a256)*', $img);

my $vtoctrk = $opts{t} // 17;
my @vtocsec = unpack('C*', $sectors[$vtoctrk*$sectrk]);
print "DISK VOLUME ", $vtocsec[6], "\n\n" if $printcat;

my @fdata;

if (defined $opts{a}) {
    my @m = split /:/, $opts{a};
    $fdata[0] = hex($m[0]);
    $fdata[1] = hex($m[1]);
    $fdata[2] = uc $m[2];
    $fdata[3] = $m[3];
} else {
    &processcatalog;
}

&readfile(@fdata) if @fdata;
  
$DB::single = 1;
1;

sub processcatalog {
    my $catsec = $vtocsec[1]*$sectrk + $vtocsec[2];
    while ($catsec) {
	my ($nexttrk, $nextsec, @entries) =
	    unpack('xCCx8(a35)7', $sectors[$catsec]);
	my $nucatsec = $nexttrk*$sectrk + $nextsec;
	# avoid infinite looping when people are naughty
	$catsec = ($nucatsec > $catsec || $nucatsec == $catsec) ? 0 : $nucatsec;
	for my $e (@entries) {
	    my ($ftrk, $fsec, $ftype, $fnamehi, $len) =
		unpack('CCCa30v', $e);
	    next if ($ftrk == 0 || $ftrk == 255);
	    my $foff = 0;
	    print (($ftype & 0x80) ? "*" : " ") if $printcat;
	    $ftype &= 0x7f;
	    while ($ftype) {$foff++; $ftype >>= 1;}
	    print $ftypes[$foff] if $printcat;
	    printf " %03d ", $len if $printcat;
	    my $fname = &fnamefix($fnamehi);
	    print $fname if $printcat;
	    if ($opts{v} && $printcat) {
		printf("   (TRK \$%02X SEC \$%X)", $ftrk, $fsec);
	    }
	    print "\n" if $printcat;
	    if (defined $fn && ($fname =~ m/^$fn\s*$/)) {
		@fdata = ($ftrk, $fsec, $ftypes[$foff], $fname);
	    }
	}
    }    
}

sub fnamefix {
    my ($dosfn) = @_;
    my @dosfn = unpack("C*", $dosfn);
    if ($opts{v}) { # do extra mangling for verbose
	my $m = "";
	for my $c (@dosfn) {
	    if ($c >= 0xa0 && $c <= 0xff) {
		$m .= chr($c & 0x7f)
	    } else {
		$m .= "{0x$c}"
	    }
	}
	return $m;
    } else {
	return pack("C*", map { $_ & 0x7f } @dosfn);
    }
}
 
sub readfile {
    my ($trk, $sec, $type, $name) = @_;
    my (@filesecs);
    $name //= "(unknown)";
    $type //= "T";
    my $nxttsl = $trk*$sectrk + $sec;
    while ($nxttsl) {
	my ($nxttrk, $nxtsec, $offs, @tslist) =
	    unpack('xCCxxvxxxxx(CC)*', $sectors[$nxttsl]);
	# what's offs for? dunno.
	while (@tslist) {
	    my $sec = $sectrk * shift @tslist;
	    $sec += shift @tslist;
	    push @filesecs, $sec if $sec;
	}
	$nxttsl = $nxttrk * $sectrk + $nxtsec;
	#$DB::single = 1;
    }
    my $firstsec = shift @filesecs;
    my ($addr, $len, $data);
    if ($type eq "B") {
	($addr, $len, $data) = unpack("vva*", $sectors[$firstsec]);
    } elsif ($type eq "A" || $type eq "I") {
	($len, $data) = unpack("va*", $sectors[$firstsec]);
	$addr = 0x801;
    } else {
	$data = $sectors[$firstsec];
    }
    $data .= join "", @sectors[@filesecs];
    if ($len) {
	$data = unpack("a$len", $data);
    }

    if ($opts{d}) { print $data; return }
    
    if ($type eq "T") {
	for my $c (unpack('C*', $data)) {
	    last if $c == 0;
	    $c &= 0x7f;
	    print (($c eq 13) ? "\n" : chr($c));
	}
    } elsif ($type eq "A") {
	&fpbasic($data, $len);
    } elsif ($type eq "I") {
	&intbasic($data, $len);
    } elsif ($type eq "B") {
	printf "Address 0x%x Length 0x%x Type %s Name %s\n",
	    $addr, $len, $type, $name;
	print encode_base64($data);
    } else {
	print "not BAIT; use -d to dump as binary.\n";
    }
}

sub fpbasic {
    my ($data) = @_;
    my @tokens = (
	'END', 'FOR', 'NEXT', 'DATA', 'INPUT', 'DEL', 'DIM', 'READ',
	'GR', 'TEXT', 'PR #', 'IN #', 'CALL', 'PLOT', 'HLIN', 'VLIN',
	'HGR2',	'HGR', 'HCOLOR=', 'HPLOT', 'DRAW', 'XDRAW', 'HTAB',
	'HOME', 'ROT=',	'SCALE=', 'SHLOAD', 'TRACE', 'NOTRACE', 'NORMAL',
	'INVERSE', 'FLASH', 'COLOR=', 'POP', 'VTAB', 'HIMEM:', 'LOMEM:',
	'ONERR', 'RESUME', 'RECALL', 'STORE', 'SPEED=', 'LET', 'GOTO',
	'RUN', 'IF', 'RESTORE', '&', 'GOSUB', 'RETURN', 'REM', 'STOP',
	'ON', 'WAIT', 'LOAD', 'SAVE', 'DEF FN', 'POKE', 'PRINT', 'CONT',
	'LIST', 'CLEAR', 'GET', 'NEW', 'TAB', 'TO', 'FN', 'SPC(', 'THEN',
	'AT', 'NOT', 'STEP', '+', '-', '*', '/', ';', 'AND', 'OR', '>',
	'=', '<', 'SGN', 'INT', 'ABS', 'USR', 'FRE', 'SCRN (', 'PDL',
	'POS', 'SQR', 'RND', 'LOG', 'EXP', 'COS', 'SIN', 'TAN', 'ATN',
	'PEEK', 'LEN', 'STR$', 'VAL', 'ASC', 'CHR$', 'LEFT$', 'RIGHT$',
	'MID$'
	);
    my @bytes = unpack("C*", $data);
    my $ptr = 0;
    my $b;
    while (1) {
	print "\n";
	my $next = $bytes[$ptr++] + 256*$bytes[$ptr++];
	last if $next == 0;
	my $lineno = $bytes[$ptr++] + 256*$bytes[$ptr++];
	print "$lineno ";
	while ($b = $bytes[$ptr++]) {
	    if ($b > 127) {
		print " ", $tokens[$b-128], " ";
	    } else {
		print chr($b);
	    }
	}
    }
}
    
sub intbasic {
    my ($data, $len) = @_;
    my @tokens = (
	'HIMEM:', "\n", '_', ': ', 'LOAD', 'SAVE', 'CON', 'RUN',
        'RUN', 'DEL ', ',', 'NEW', 'CLR', 'AUTO', ',', 'MAN', 'HIMEM:',
	'LOMEM:', '+', '-', '*', '/', '=', '#', '>=', '>', '<=', '<>',
	'<', ' AND ', ' OR ', ' MOD ', '^', '+', '(', ',', ' THEN ', ' THEN ',
	',', ',', '"', '"', '(', '!', '!', '(', 'PEEK ', 'RND', 'SGN',
	'ABS', 'PDL', 'RNDX', '(', '+', '-', 'NOT', '(', '=', '#', 'LEN(',
	'ASC(', 'SCRN(', ',', '(', '$', '$', '(', ',', ',', ';', ';', ';',
        ',', ',', ',', 'TEXT', 'GR', 'CALL ', 'DIM ', 'DIM ', 'TAB ', 'END',
	'INPUT', 'INPUT', 'INPUT', 'FOR ', '=', ' TO ', ' STEP ', 'NEXT ', ',',
	'RETURN', 'GOSUB ', 'REM ', 'LET ', 'GOTO ', 'IF ', 'PRINT ', 'PRINT ',
	'PRINT ', 'POKE ', ',', 'COLOR=','PLOT ', ',', 'HLIN ', ',', ' AT ',
        'VLIN ', ',', ' AT ', 'VTAB ', '=', '=', ')', ')', 'LIST', ',', 'LIST',
	'POP', 'NODSP', 'DSP', 'NOTRACE','DSP', 'DSP', 'TRACE', 'PR#', 'IN#',
	);

    my @bytes = unpack("C$len", $data);
    push @bytes, 0;
    my $ptr = 0;
    my $b;
    $DB::single = 1;
    while (1) {
	my $len = $bytes[$ptr];
	last if $len == 0;
	my @ldata = @bytes[($ptr+3)..($ptr+$len-1)];
	my $lineno = $bytes[$ptr+1] + 256*$bytes[$ptr+2];
	print "$lineno ";
	if (defined $opts{v}) {
	    print "[ ";
	    for my $i (@ldata) {
		printf "%02x ", $i;
	    }
	    print "]";
	}

	my $rem = 0;
	my $quote = 1;
	# the ways in which numbers are stored are somewhat insane (and
	# context dependent); see Paul Schlyter's c.s.a2 'integer basic
	# tokenization' post. This code cheats a bit.
	while (scalar @ldata) {
	    $b = shift @ldata;
	    if ($b & 0x80) {
		if (!$quote && !$rem && $b >= 0xb0 && $b <= 0xbf) {
		    print 0+((shift @ldata) + (256*(shift @ldata)));
		} else {
		    print chr($b & 0x7f);
		    $quote = 1;
		}
	    } else {
		if ($b == 0x28) {
		    $quote = !$quote;
		} else {
		    $quote = 0;
		}
		if ($b == 0x5d) { # REM; rest of the line *should* be text.
		    $rem = 1;
		}
	    }
	    print $tokens[$b];
	}
	$ptr += $len;
    }
}

sub help {
    print STDERR "
$0 [-3] [-a t:s:f:n] [-c] [-d] [-t 17] [-v] disk.dsk [FILENAME]
  -a 22:f:T : bypass catalog, dump file at T\$22 S\$F as filetype T
  -3 : 13 sectors/track
  -c : show catalog before printing
  -d : dump file verbatim, no decoding
  -t 17 : change catalog track to 17
  -v : extra verbosity
";
    exit 0;
}
