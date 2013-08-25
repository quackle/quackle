#!/usr/bin/perl

# USAGE: generate_raw_words.pl generate_words_output

binmode STDOUT, ':utf8';

sub read_and_spit {
	for my $arg (@ARGV) {
		open (my $input, "<:encoding(utf8)", $arg);

		while (<$input>) {
			chomp;
			my ($word, $count) = split(/\s/, $_);

			my $clean_word = $word;
			$clean_word =~ s/;//g;

			print "$clean_word\n";
		}
	}
}

read_and_spit();
