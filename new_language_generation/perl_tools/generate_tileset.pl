#!/usr/bin/perl

use warnings;
use strict;

use Getopt::Long;
use Pod::Usage;

my $alphabet_filename = 'undef';
my $help = 0;

GetOptions('alphabet=s' => \$alphabet_filename,
		   'help|?' => \$help) or pod2usage(2);

pod2usage(1) if $help;

=pod

=head1 NAME

generate_tileset.pl - Print out HTML tileset

=head1 SYNOPSIS

--alphabet=<file>  
	output of generate_alphabet.pl

=cut

binmode STDOUT, ':utf8';

# tiles{count}{letter} = score
my %tiles;

sub read_alphabet {
	if ($alphabet_filename eq 'undef') {
		return;
	}

	open (my $input, "<:encoding(utf8)", $alphabet_filename);

	my $i = 0;
	while (<$input>) {
		chomp;
		next if (/^\#/);

		my ($letter, $blank_text, $score, $count) = split /\s/, $_;

		if ($letter eq 'blank') {
			$tiles{$score}{"&#12288;"} = 0;
			next;
		}

		next if (! defined $count);

		$letter =~ s/\|//g;
		$tiles{$count}{$letter} = $score;
	}
}

sub spit_characters {
	my $fontsize = "1.2cm";
	my $score_fontsize = "18pt";
	#my $color="#458B74";
	#my $color="black";
	#my $color="#EAC117";
	my $color="darkblue";
	my $width = "5in";
	my $height = "0";

	print "<html>\n<head>\n<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\">\n</head>\n<body bgcolor=white>\n";

	print "<table cellspacing=2 cellpadding=0>\n";

	print "<tr>";
	my $already_in_row = 0;
	for my $count (sort keys %tiles) {
		for my $letter (sort keys %{$tiles{$count}}) {
			for my $i (1 .. $count) {
				if ($already_in_row >= 10) {
					print "</tr><tr>\n";
					$already_in_row = 0;
				}

				my $score = $tiles{$count}{$letter};
				my $align = 'center';
				print "<td width=$width height=$height align=$align><nobr>\n";
				print "<span style=\"font-size: $fontsize; color: $color\">$letter</span>";
				print "<span style=\"font-size: $score_fontsize; color: $color\"><sub>$score</sub></span>";
				print "</nobr></td>\n";

				++$already_in_row;
			}
		}
	}
	print "</tr>\n";

	print "</table>\n";
	print "</body>\n</html>\n";
}

read_alphabet();
spit_characters();
