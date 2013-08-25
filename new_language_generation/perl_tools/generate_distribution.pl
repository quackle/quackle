#!/usr/bin/perl

# USAGE: generate_distribution.pl word_list > character_list

use warnings;
use strict;

use Getopt::Long;
use Pod::Usage;

my $help = 0;

GetOptions(
           'help|?' => \$help) or pod2usage(2);

pod2usage(1) if $help;

=pod

=head1 NAME

generate_distribution.pl - Generate letter distribution data

=head1 SYNOPSIS

One argument: the output of generate_words.pl.

=cut

binmode STDOUT, ':utf8';

my %characters;

sub read_characters {
	for my $arg (@ARGV) {
		open (my $input, "<:encoding(utf8)", $arg);

		while (<$input>) {
			chomp;
			my ($word, $count) = split(/\s/, $_);

			for my $character (split(/;/, $word)) {
				$characters{$character} += $count;
			}
		}
	}
}

sub spit_characters {
	my @sorted_characters = sort { $characters{$b} <=> $characters{$a} } keys %characters;
	for my $character (@sorted_characters) {
		print "$character $characters{$character}\n";
	}
}

read_characters();
spit_characters();
