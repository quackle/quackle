#!/usr/bin/perl

use warnings;
use strict;
use Encode qw/encode decode/; 
use Getopt::Long;
use Pod::Usage;
use Unicode::Normalize;
use Lingua::KO::Hangul::Util qw(:all);

my $granularity = 'letter';
my $input_encoding = 'utf8';
my $split_at_spaces_only = 0;
my $help = 0;

GetOptions('granularity=s' => \$granularity,
           'input_encoding=s' => \$input_encoding,
           'split_at_spaces_only' => \$split_at_spaces_only,
		   'help|?' => \$help) or pod2usage(2);

pod2usage(1) if $help;

=pod

=head1 NAME

generate_words.pl - Generate a word list

=head1 SYNOPSIS

--granularity=<letter|jamo>  
	how big a letter is. 

--input_encoding=<utf8|euc-kr>
	incoding of input texts

=cut

my %words;

binmode STDOUT, ':utf8';

sub read_words {
	@ARGV = map { /\.(gz|Z)$/ ? "gzip -dc $_ |" : $_  } @ARGV;

	while (<ARGV>) {
		chomp;
		my @words;
		if ($split_at_spaces_only) {
			@words = split(/ /, $_);
		} else {
			@words = split(/[\s\w\-\)\(\,\.\;\:\!\"\~\=\'\`\%\<\>\[\]\/\‘\’\?\#\@]/, $_);
		}

		for my $word (@words) {
			my $utf8_word;
			$utf8_word = decode($input_encoding, $word);

			if (!defined $utf8_word || length($utf8_word) == 0) {
				next;
			}

			my $normalized_utf8_word = uc($utf8_word);

			my $expanded_word = expand_word($normalized_utf8_word);

			if (length $expanded_word > 0) {
				$words{$expanded_word}++;
			}
		}
	}
}

sub expand_word {
	my ($word) = @_;
	my @ret;

	if ($granularity eq 'jamo') {
		@ret = expand_jamo($word);
	} elsif ($granularity eq 'letter') {
		@ret = split //, $word;
	} else {
		die "Granularity '$granularity' is not recognized.\n";
	}

	return join ';', @ret;
}

sub expand_jamo {
	my ($word) = @_;

	if (length($word) == 0) {
		return $word;
	}

	my @syllables = split(//, $word);
	my @cumulative_jamos; 

	for my $syllable (@syllables) {
		my $decomposed_string = decomposeSyllable($syllable);

		for my $jamo (split(//, $decomposed_string)) {
			push @cumulative_jamos, normalize_jamo($jamo);
		}
	}

	return @cumulative_jamos;
}

sub spit_words {
	for my $word (sort keys %words) {
		print "$word $words{$word}\n" if (length($word) > 1);
	}
}

sub execute {
	read_words();
	spit_words();
}

# Following is Hangul-specific code and then an execute() call.
# Constants and ideas borrowed from Lingua::KO::Hangul::Util.

use constant SBase  => 0xAC00;
use constant SFinal => 0xD7A3; # SBase -1 + SCount
use constant SCount =>  11172; # LCount * NCount
use constant NCount =>    588; # VCount * TCount
use constant LBase  => 0x1100;
use constant LFinal => 0x1112;
use constant LCount =>     19; # scalar @JamoL
use constant VBase  => 0x1161;
use constant VFinal => 0x1175;
use constant VCount =>     21; # scalar @JamoV
use constant TBase  => 0x11A7;
use constant TFinal => 0x11C2;
use constant TCount =>     28; # scalar @JamoT
use constant JBase  => 0x1100;
use constant JFinal => 0x11FF;
use constant JCount =>    256;

use constant JamoLIni   => 0x1100;
use constant JamoLFin   => 0x1159;
use constant JamoLFill  => 0x115F;
use constant JamoVIni   => 0x1160;
use constant JamoVFin   => 0x11A2;
use constant JamoTIni   => 0x11A8;
use constant JamoTFin   => 0x11F9;

my @JamoL = ( # Initial (HANGUL CHOSEONG)
    "G", "GG", "N", "D", "DD", "R", "M", "B", "BB",
    "S", "SS", "", "J", "JJ", "C", "K", "T", "P", "H",
  );

my @JamoV = ( # Medial  (HANGUL JUNGSEONG)
    "A", "AE", "YA", "YAE", "EO", "E", "YEO", "YE", "O",
    "WA", "WAE", "OE", "YO", "U", "WEO", "WE", "WI",
    "YU", "EU", "YI", "I",
  );

my @JamoT = ( # Final    (HANGUL JONGSEONG)
    "", "G", "GG", "GS", "N", "NJ", "NH", "D", "L", "LG", "LM",
    "LB", "LS", "LT", "LP", "LH", "M", "B", "BS",
    "S", "SS", "NG", "J", "C", "K", "T", "P", "H",
  );

my @TtoL = (
    "", # ""
	"G", # "G"
	"G;G", # "GG"
	"G;S", # "GS"
	"N", # "N"
	"N;J", # "NJ"
	"N;H", # "NH"
	"D", # "D"
	"R", # "L"
	"R;G", # "LG"
	"R;M", # "LM"
    "R;B", # "LB"
	"R;S", # "LS"
	"R;T", # "LT"
	"R;P", # "LP"
	"R;H", # "LH"
	"M", # "M"
	"B", # "B"
	"B;S", # "BS"
    "S", # "S"
	"SS", # "SS"
	"", # "NG"
	"J", # "J"
	"C", # "C"
	"K", # "K"
	"T", # "T"
	"P", # "P"
	"H", # "H"
);

my(%CodeL, %CodeV, %CodeT);
@CodeL{@JamoL} = 0 .. LCount-1;
@CodeV{@JamoV} = 0 .. VCount-1;
@CodeT{@JamoT} = 0 .. TCount-1;

# Normalizes jamo then inserts fillers

sub normalize_jamo {
	my ($jamo) = @_;

	my @homogenized = convert_trailing_to_leading($jamo);

	my @homogenized_filled;
	for my $homogenous_jamo_codepoint (@homogenized) {
		my $filled = fill_jamo($homogenous_jamo_codepoint);
		push @homogenized_filled, $filled;
	}

	return @homogenized_filled;
}

# Takes a L, V, or T jamo codepoint, fills in the rest,
# and returns a unicode string.
sub fill_jamo {
	my ($codepoint) = @_;
	my $syllable_type = getSyllableType($codepoint);

	# technique = jamo_alone, raw_jamo, romanize, jamo_letter, or crazy
	my $technique = 'jamo_letter';

	if ($technique eq 'raw_jamo') {
		return '|' . pack('U*', $codepoint) . '|';
	}

	if ($technique eq 'jamo_alone') {
		if ($syllable_type eq 'T') {
			return "\x{25CB}";
		}
		return insertFiller(pack('U*', $codepoint));
	}

	if ($technique eq 'romanize') {
		return romanize_codepoint($codepoint);
	}

	my $leading;
	my $vowel;
	my $trailing = 0;

	if ($syllable_type eq 'L') {
		$leading = $codepoint - JamoLIni;
		$vowel = $CodeV{'A'};
	} elsif ($syllable_type eq 'V') {
		$leading = $CodeL{''};
		$vowel = $codepoint - JamoVIni - 1;
	} elsif ($syllable_type eq 'T') {
		# Always the circle.
		$leading = $CodeL{''};
		$vowel = $CodeV{'A'};
		$trailing = $CodeT{'NG'};
	} else {
		return pack('U*', $codepoint);
	}

    return pack('U*', SBase + $leading * NCount + $vowel * TCount + $trailing);
}

sub romanize_codepoint {
	my ($codepoint) = @_;

	my $roman;

	my $syllable_type = getSyllableType($codepoint);
	if ($syllable_type eq 'L') {
		$roman = $JamoL[$codepoint - JamoLIni];
	} elsif ($syllable_type eq 'V') {
		$roman = $JamoV[$codepoint - JamoVIni - 1];
	} elsif ($syllable_type eq 'T') {
		$roman = "NG";
	} else {
		return pack('U*', $codepoint);
	}

	return "|$roman|";
}

# Converts a trailing consonant to a string of leading ones.
# Returns an array of codepoints.
sub convert_trailing_to_leading {
	my ($jamo) = @_;
	my @ret_codepoints = unpack('U*', $jamo);

	die 'normalize_jamo called with more than one codepoint' if (@ret_codepoints > 1);
	my $codepoint = $ret_codepoints[0];

	# Elide placeholder consonant.
	if (@ret_codepoints == 1 && $ret_codepoints[0] == JamoLIni + $CodeL{''}) {
		return ();
	}

	if (getSyllableType($codepoint) eq 'T') {
		my $Tindex = $codepoint - JamoTIni + 1;

		my @leaders = split /;/, $TtoL[$Tindex];
		push @leaders, '' if (@leaders == 0);  # '' gets lost otherwise

		@ret_codepoints = ();
		for my $leader (@leaders) {
			my $new_codepoint = JamoLIni + $CodeL{$leader};
			push @ret_codepoints, $new_codepoint;
		}
	}

	if (@ret_codepoints == 1 && $ret_codepoints[0] == JamoLIni + $CodeL{''}) {
		$ret_codepoints[0] = JamoTIni + $CodeT{'NG'} - 1;
	}

	return @ret_codepoints;
}

execute();
