#!/usr/bin/perl

# USAGE: analyse_gcgs.pl *.gcg > gcganalysis

use warnings;
use strict;

my %letter_points;
my %letter_counts;

binmode STDOUT, ':utf8';

sub read_gcgs {
	my $games_count = 0;
	for my $arg (@ARGV) {
		open (my $input, "<:encoding(utf8)", $arg);
		++$games_count;

		while (<$input>) {
			chomp;
			next if (!/^\>/);

			my ($player, $rack, $position, $play, $score, $cumulativescore) = split /\s+/, $_;

			next if (! defined $cumulativescore);
			if ($play =~ /\[/) {
				print "blank play! $play for $score\n";
				next;
			}

			for my $letter (split /|/, $play) {
				++$letter_counts{$letter};
				$letter_points{$letter} += int($score);

				#print "$letter now seen " . $letter_counts{$letter} . " times, scored " . $letter_points{$letter} . " points\n";
			}
		}
	}

	print "# Read $games_count games.\n";
}

sub spit_analysis {
	my %points_per_turn;
	for my $letter (keys %letter_counts) {
		$points_per_turn{$letter} = $letter_points{$letter} / $letter_counts{$letter};
	}

	for my $letter (sort { $points_per_turn{$b} <=> $points_per_turn{$a} } keys %points_per_turn) {
		# TODO lame that this always puts out un-bar-surrounded letters
		print "$letter " . $points_per_turn{$letter} . "\n";
	}
}

read_gcgs();
spit_analysis();
