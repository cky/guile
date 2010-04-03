#!/usr/bin/perl
# unidata_to_charset.pl --- Compute SRFI-14 charsets from UnicodeData.txt
#
# Copyright (C) 2009, 2010 Free Software Foundation, Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

open(my $in,  "<",  "UnicodeData.txt")  or die "Can't open UnicodeData.txt: $!";           
open(my $out, ">",  "srfi-14.i.c") or die "Can't open srfi-14.i.c: $!";

# For Unicode, we follow Java's specification: a character is
# lowercase if
#    * it is not in the range [U+2000,U+2FFF], and
#    * the Unicode attribute table does not give a lowercase mapping
#      for it, and
#    * at least one of the following is true:
#          o the Unicode attribute table gives a mapping to uppercase
#            for the character, or
#          o the name for the character in the Unicode attribute table
#            contains the words "SMALL LETTER" or "SMALL LIGATURE".

sub lower_case {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if (($codepoint < 0x2000 || $codepoint > 0x2FFF)
        && (!defined($lowercase) || $lowercase eq "")
        && ((defined($uppercase) && $uppercase ne "")
            || ($name =~ /(SMALL LETTER|SMALL LIGATURE)/))) {
        return 1;
    } else {
        return 0;
    }
}

# For Unicode, we follow Java's specification: a character is
# uppercase if
#    * it is not in the range [U+2000,U+2FFF], and
#    * the Unicode attribute table does not give an uppercase mapping
#      for it (this excludes titlecase characters), and
#    * at least one of the following is true:
#          o the Unicode attribute table gives a mapping to lowercase
#            for the character, or
#          o the name for the character in the Unicode attribute table
#            contains the words "CAPITAL LETTER" or "CAPITAL LIGATURE".

sub upper_case {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if (($codepoint < 0x2000 || $codepoint > 0x2FFF)
        && (!defined($uppercase) || $uppercase eq "")
        && ((defined($lowercase) && $lowercase ne "")
            || ($name =~ /(CAPITAL LETTER|CAPITAL LIGATURE)/))) {
        return 1;
    } else {
        return 0;
    }
}

# A character is titlecase if it has the category Lt in the character
# attribute database.

sub title_case {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if (defined($category) && $category eq "Lt") {
        return 1;
    } else {
        return 0;
    }
}

# A letter is any character with one of the letter categories (Lu, Ll,
# Lt, Lm, Lo) in the Unicode character database.

sub letter {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if (defined($category) && ($category eq "Lu"
                               || $category eq "Ll"
                               || $category eq "Lt"
                               || $category eq "Lm"
                               || $category eq "Lo")) {
        return 1;
    } else {
        return 0;
    }
}

# A character is a digit if it has the category Nd in the character
# attribute database. In Latin-1 and ASCII, the only such characters
# are 0123456789. In Unicode, there are other digit characters in
# other code blocks, such as Gujarati digits and Tibetan digits.

sub digit {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if (defined($category) && $category eq "Nd") {
        return 1;
    } else {
        return 0;
    }
}

# The only hex digits are 0123456789abcdefABCDEF. 

sub hex_digit {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if (($codepoint >= 0x30 && $codepoint <= 0x39)
        || ($codepoint >= 0x41 && $codepoint <= 0x46)
        || ($codepoint >= 0x61 && $codepoint <= 0x66)) {
        return 1;
    } else {
        return 0;
    }
}

# The union of char-set:letter and char-set:digit.

sub letter_plus_digit {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if (letter($codepoint, $name, $category, $uppercase, $lowercase)
        || digit($codepoint, $name, $category, $uppercase, $lowercase)) {
        return 1;
    } else {
        return 0;
    }
}

# Characters that would 'use ink' when printed
sub graphic {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if ($category =~ (/L|M|N|P|S/)) {
        return 1;
    } else {
        return 0;
    }
}

# A whitespace character is either
#    * a character with one of the space, line, or paragraph separator
#      categories (Zs, Zl or Zp) of the Unicode character database.
#    * U+0009 Horizontal tabulation (\t control-I)
#    * U+000A Line feed (\n control-J)
#    * U+000B Vertical tabulation (\v control-K)
#    * U+000C Form feed (\f control-L)
#    * U+000D Carriage return (\r control-M)

sub whitespace {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if ($category =~ (/Zs|Zl|Zp/)
        || $codepoint == 0x9
        || $codepoint == 0xA 
        || $codepoint == 0xB 
        || $codepoint == 0xC 
        || $codepoint == 0xD) { 
        return 1;
    } else {
        return 0;
    }
}

# A printing character is one that would occupy space when printed,
# i.e., a graphic character or a space character. char-set:printing is
# the union of char-set:whitespace and char-set:graphic.

sub printing {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if (whitespace($codepoint, $name, $category, $uppercase, $lowercase)
        || graphic($codepoint, $name, $category, $uppercase, $lowercase)) {
        return 1;
    } else {
        return 0;
    }
}

# The ISO control characters are the Unicode/Latin-1 characters in the
# ranges [U+0000,U+001F] and [U+007F,U+009F].

sub iso_control {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if (($codepoint >= 0x00 && $codepoint <= 0x1F)
        || ($codepoint >= 0x7F && $codepoint <= 0x9F)) {
        return 1;
    } else {
        return 0;
    }
}

# A punctuation character is any character that has one of the
# punctuation categories in the Unicode character database (Pc, Pd,
# Ps, Pe, Pi, Pf, or Po.)

# Note that srfi-14 gives conflicting requirements!!  It claims that
# only the Unicode punctuation is necessary, but, explicitly calls out
# the soft hyphen character (U+00AD) as punctution.  Current versions
# of Unicode consider U+00AD to be a formatting character, not
# punctuation.

sub punctuation {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if ($category =~ (/P/)) {
        return 1;
    } else {
        return 0;
    }
}
        
# A symbol is any character that has one of the symbol categories in
# the Unicode character database (Sm, Sc, Sk, or So).

sub symbol {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if ($category =~ (/S/)) {
        return 1;
    } else {
        return 0;
    }
}
        
# Blank chars are horizontal whitespace.  A blank character is either
#    * a character with the space separator category (Zs) in the
#      Unicode character database.
#    * U+0009 Horizontal tabulation (\t control-I) 
sub blank {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if ($category =~ (/Zs/)
        || $codepoint == 0x9) { 
        return 1;
    } else {
        return 0;
    }
}

# ASCII
sub ascii {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if ($codepoint <= 0x7F) {
        return 1;
    } else {
        return 0;
    }
}

# Empty
sub empty {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    return 0;
}

# Designated -- All characters except for the surrogates
sub designated {
    my($codepoint, $name, $category, $uppercase, $lowercase)= @_;
    if ($category =~ (/Cs/)) {
        return 0;
    } else {
        return 1;
    }
}


# The procedure generates the two C structures necessary to describe a
# given category.
sub compute {
    my($f) = @_;
    my $start = -1;
    my $end = -1;
    my $len = 0;
    my @rstart = (-1);
    my @rend = (-1);

    seek($in, 0, 0) or die "Can't seek to beginning of file: $!";

    print "$f\n";

    while (<$in>) {
        # Parse the 14 column, semicolon-delimited UnicodeData.txt
        # file
        chomp;
        my(@fields) = split(/;/);

        # The codepoint: an integer
        my $codepoint = hex($fields[0]); 

        # If this is a character range, the last character in this
        # range
        my $codepoint_end = $codepoint;  

        # The name of the character
        my $name = $fields[1];    

        # A two-character category code, such as Ll (lower-case
        # letter)
        my $category = $fields[2];       

        # The codepoint of the uppercase version of this char
        my $uppercase = $fields[12];   

        # The codepoint of the lowercase version of this char
        my $lowercase = $fields[13];    

        my $pass = &$f($codepoint,$name,$category,$uppercase,$lowercase);
        if ($pass == 1) {

            # Some pairs of lines in UnicodeData.txt delimit ranges of
            # characters.
            if ($name =~ /First/) {
                $line = <$in>;
                die $! if $!;
                $codepoint_end = hex( (split(/;/, $line))[0] );
            }                 

            # Compute ranges of characters [start:end] that meet the
            # criteria.  Store the ranges.
            if ($start == -1) {
                $start = $codepoint;
                $end = $codepoint_end;
            } elsif ($end + 1 == $codepoint) {
                $end = $codepoint_end;
            } else {
                $rstart[$len] = $start;
                $rend[$len] = $end;
                $len++;
                $start = $codepoint;
                $end = $codepoint_end;
            }
        }
    }

    # Extra logic to ensure that the last range is included
    if ($start != -1) {
        if ($len > 0 && $rstart[@rstart-1] != $start) {
            $rstart[$len] = $start;
            $rend[$len] = $end;
            $len++;
        } elsif ($len == 0) {
	    $rstart[0] = $start;
	    $rend[0] = $end;
	    $len++;
        }
    }

    # Print the C struct that contains the range list.
    print $out "scm_t_char_range cs_" . $f . "_ranges[] = {\n";
    if ($rstart[0] != -1) {
        for (my $i=0; $i<@rstart-1; $i++) {
            printf $out "  {0x%04x, 0x%04x},\n", $rstart[$i], $rend[$i];
        }
        printf $out "  {0x%04x, 0x%04x}\n", $rstart[@rstart-1], $rend[@rstart-1];
    }
    print $out "};\n\n";

    # Print the C struct that contains the range list length and
    # pointer to the range list.
    print $out "scm_t_char_set cs_${f} = {\n";
    print $out "  $len,\n";
    print $out "  cs_" . $f . "_ranges\n";
    print $out "};\n\n";
}

# Write a bit of a header
print $out "/* srfi-14.i.c -- standard SRFI-14 character set data */\n\n";
print $out "/* This file is #include'd by srfi-14.c.  */\n\n";
print $out "/* This file was generated from\n";
print $out "   http://unicode.org/Public/UNIDATA/UnicodeData.txt\n";
print $out "   with the unidata_to_charset.pl script.  */\n\n";

# Write the C structs for each SRFI-14 charset
compute "lower_case";
compute "upper_case";
compute "title_case";
compute "letter";
compute "digit";
compute "hex_digit";
compute "letter_plus_digit";
compute "graphic";
compute "whitespace";
compute "printing";
compute "iso_control";
compute "punctuation";
compute "symbol";
compute "blank";
compute "ascii";
compute "empty";
compute "designated";

close $in;
close $out;

exec ('indent srfi-14.i.c') or print STDERR "call to 'indent' failed: $!";

# And we're done.






