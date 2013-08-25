#!/usr/bin/perl

use warnings;
use strict;

use Getopt::Long;
use Pod::Usage;

use Lingua::KO::Hangul::Util qw(:all);
use POSIX qw(ceil floor);

my $distribution_filename = 'undef';
my $analysis_filename = 'undef';
my $count_assigner_filename = 'undef';

my $counts_only = 0;

my $number_of_distinct_tiles = 30;
my $number_of_blanks = 2;

my $help = 0;

GetOptions('distribution=s' => \$distribution_filename,
           'analysis=s' => \$analysis_filename,
           'count_assigner=s' => \$count_assigner_filename,
           'number_of_distinct_tiles=i' => \$number_of_distinct_tiles,
           'number_of_blanks=i' => \$number_of_blanks,
           'counts_only' => \$counts_only,
		   'help|?' => \$help) or pod2usage(2);

pod2usage(1) if $help;

=pod

=head1 NAME

generate_alphabet.pl - Print out quackle alphabet file

=head1 SYNOPSIS

--distribution=<file>  
	output of generate_distribution.pl

--analysis=<file>  
	output of analyse_gcgs.pl

--count_assigner=<file>
	file with one number per line. Number on nth line is how many
	tiles are in bag of letter that appears nth least often.

--number_of_distinct_tiles=[1, 30]
	how many tiles are in the alphabet (should be same as length
	of count assigner file).

--number_of_blanks=<nonnegative integer>
	how many scoreless blanks are in the bag.

--counts_only
	only output the counts of letters, in same format as
	count_assigner input

=cut

my @count_assigner = ();
my $has_assigned_counts = 0;

my %character_counts;
my $count_sum = 0;
my $inverse_count_sum = 0;

my %character_points_per_appearance;
my $lowest_points_per_appearance = -1;
my $highest_points_per_appearance = 0;
my $maximum_value = 10;

binmode STDOUT, ':utf8';

sub read_count_assigner {
	if ($count_assigner_filename eq 'undef') {
		$has_assigned_counts = 0;
		return;
	}

	if (!(-e $count_assigner_filename)) {
		print STDERR "Count assigner $count_assigner_filename does not exist; ignoring.\n";
		$has_assigned_counts = 0;
		return;
	}

	$has_assigned_counts = 1;

	open (my $input, "<:encoding(utf8)", $count_assigner_filename);

	my $i = 0;
	while (<$input>) {
		chomp;
		next if (/^\#/);

		my ($count, @rest) = split /\s/, $_;
		$count_assigner[$i] = $count;
		++$i;
	}
}

sub read_analysis {
	if ($analysis_filename eq 'undef') {
		return;
	}

	if (!(-e $analysis_filename)) {
		print STDERR "Analysis $analysis_filename does not exist; ignoring.\n";
		return;
	}

	open (my $input, "<:encoding(utf8)", $analysis_filename);

	my $i = 0;
	while (<$input>) {
		chomp;
		next if (/^\#/);

		my ($letter, $points_per_appearance) = split /\s/, $_;
		next if (! defined $points_per_appearance);

		next if ($letter =~ /\[/);
		next if ($letter =~ /\]/);

		$letter =~ s/\|//g;

		$character_points_per_appearance{$letter} = $points_per_appearance;
	}

	calculate_bounding_points_per_appearance_values();
}

sub calculate_bounding_points_per_appearance_values {
	my $i = 0;
	for my $character (sort { $character_counts{$a} <=> $character_counts{$b} } keys %character_counts) {
		if (exists $character_counts{$character} && count_of_character($i, $character) > 0) {
			my $points_per_appearance = $character_points_per_appearance{$character};
			if ($lowest_points_per_appearance < 0 || $points_per_appearance < $lowest_points_per_appearance) {
				$lowest_points_per_appearance = $points_per_appearance;
			}

			if ($points_per_appearance > $highest_points_per_appearance) {
				$highest_points_per_appearance = $points_per_appearance;
			}
		}

		++$i;
	}
}

sub read_characters {
	open (my $input, "<:encoding(utf8)", $distribution_filename);

	my $i = 0;
	while (<$input>) {
		last if ($i >= $number_of_distinct_tiles);

		chomp;
		my ($letter, $count) = split(/\s/, $_);

		$character_counts{$letter} = $count;
		$count_sum += $count;
		$inverse_count_sum += exp(-.0005*$count);
		++$i;
	}
}

sub max {
	return $_[0] > $_[1]? $_[0] : $_[1];
}

sub value_of_character {
	my ($letter) = @_;
	$letter =~ s/\|//g;

	if (exists $character_points_per_appearance{$letter}) {
		my $value_multiplier = $maximum_value / ($highest_points_per_appearance - $lowest_points_per_appearance);

		return max(floor(($highest_points_per_appearance - $character_points_per_appearance{$letter}) * $value_multiplier), 1);
	}

	# This was written VERY late at night.
	#print "computing value of $letter; count_sum=$count_sum; my count=$character_counts{$letter}\n";
	return max(floor(187 * exp(-.0005*$character_counts{$letter}) / $inverse_count_sum), 1);
}

sub hallucinate_assigned_counts {
	my $total_rounded_count = $number_of_blanks;
	my $i = 0;
	for my $character (sort { $character_counts{$a} <=> $character_counts{$b} } keys %character_counts) {
		my $original_count = $character_counts{$character};
		my $rounded_count;

		my $truth = 100 * $original_count / $count_sum;
		if ($truth < 0.1) {
			$rounded_count = 0;
		} else {
			$rounded_count = max(floor($truth), 1);
		}

		$total_rounded_count += $rounded_count;
		$count_assigner[$i] = $rounded_count;

		++$i;
	}

	$has_assigned_counts = 1;
}

sub count_of_character {
	my ($index, $count) = @_;

	if ($has_assigned_counts && $index < @count_assigner) {
		return $count_assigner[$index]
	}

	return 0;
}

sub spit_characters {
	if ($counts_only) {
		my $i = 0;
		for my $character (sort { $character_counts{$a} <=> $character_counts{$b} } keys %character_counts) {
			my $count = count_of_character($i, $character_counts{$character});
			print "$count #" . ($i + 1) . ": $character\n";
			++$i;
		}
		return;
	}

	my $total_score = 0;
	my $total_count = $number_of_blanks;
	print "blank 0 $number_of_blanks\n";

	print "#char\tblank\tscore\tcount\tvowel\n";
	my $i = 0;
	for my $character (sort { $character_counts{$a} <=> $character_counts{$b} } keys %character_counts) {
		my $blank_text = lc($character);

		# TODO lower-case blanks currently broken.
		my $disable_lower_case_blanks = 1;
		if ($disable_lower_case_blanks || $blank_text eq $character) {
			$blank_text = '['.$character.']';
			$blank_text =~ s/\|//g;
		}

		my $score = value_of_character($character);
		my $count = count_of_character($i, $character_counts{$character});
		$total_count += $count;
		$total_score += $score;

		my $is_vowel = '0';

		# TODO make work for unfilled jamo
		if (isStandardForm($character)) {
			my @codepoints = unpack('U*', $character);
			if ($codepoints[0] == 0x115F) {
				$is_vowel = '1';
			}
		}

		++$i;

		print "$character\t$blank_text\t$score\t$count\t$is_vowel\n";
	}

	print "# Total count: $total_count\n";
	print "# Total score: $total_score\n";
}

read_characters();
read_count_assigner();
if (!$has_assigned_counts) {
	hallucinate_assigned_counts();
}

read_analysis();
spit_characters();
