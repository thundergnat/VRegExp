#!/usr/bin/perl

## VRegExp - Visual Regex Explorer #####################
#
# Author: Stephen Schulze (thundergnat)
#
# An extention/modification of a regex tweaking utility
# had posted on perlmonks.org in Sept 2011.
#
# http://www.perlmonks.org/?node_id=927175
#
########################################################

use warnings;
use strict;
use charnames ':full', ':short';
use 5.10.0; # using given/when

use Tk;
use Tk::TextUndo;
use Tk::ROText;
use Tk::Entry;
use Tk::Pane;
use Tk::Balloon;
use Tk::FontDialog;
use Tk::Dialog;

use YAPE::Regex::Explain;
use YAML qw(DumpFile LoadFile);

my $settings_file = $0 . '.settings';
my ( $lastterm, $lasttext ) = ( '', '' );

sub DEBUG () { return 0 }

my $OS;
given ($^O) {
    when (/Win32/) { $OS = 'Win32' };
    default        { $OS = 'Linux' };
};

my %settings = (
    regex => '\b((\w)\w*\2)\b',
    text  => 'Put some text in this window to match against. '
      . 'Match words that start and end with the same letter.',
    font => {    # 0: text boxes, 1: menus, 2: Other
        Win32 => [ '{Courier New} 10', '{Courier New} 10', '{Arial} 10' ],
        Linux => [ '{Monospace} 10',   '{Monospace} 8',    '{Monospace} 10' ]
        ,        # Not Win32
    },
    geometry => '650x480',
    sash     => [ [ 2, 80 ], [ 2, 240 ] ],
    bg       => {
        err       => '#FFC0C0',
        ok        => 'green',
        highlight => 'yellow',
    }
);

if ( -e $settings_file ) {
    %settings = LoadFile($settings_file);
}

my %flag = (    # regex flags
    case     => '', # i
    multiple => '', # m
    single   => '', # s
    ws       => '', # x
    global   => 1   # g
);

my $update;
my $error_text;
my $matches  = 'Matches: ';
my $cap_disp = '';
my @show     = ( undef, 1 );    # Array of capture display flags
                                # Show first capture by default

my %w;                          # Hash to hold tk widgets;

$w{mw} = MainWindow->new;

$w{help} = $w{mw}->Balloon( -initwait => 1000 );

$w{fd} = $w{mw}->FontDialog(
    -nicefont => 0,
    -title    => 'Select Font',
    #-applycmd        => sub {},
    -familylabel      => 'Font Family',
    -fixedfontsbutton => 1,
    -nicefontsbutton  => 1,
);

## Set up frames for the various sections
$w{top_frame} = $w{mw}->Frame->pack(
    -side   => 'top',
    -fill   => 'x',
    -expand => 1,
    -anchor => 'nw'
);

$w{paned_window} = $w{mw}->Panedwindow( -orient => 'vertical' )->pack(
    -side   => 'top',
    -expand => 'yes',
    -anchor => 'n',
    -fill   => 'both'
);

$w{result_frame} = $w{mw}->Frame;

## Set up top frame widgets
$w{top_frame}->Label( -text => 'Modifiers:' )->pack( -side => 'left' );

$w{ck_bt_case} = $w{top_frame}->Checkbutton(
    -text     => 'i',
    -onvalue  => 'i',
    -offvalue => '',
    -variable => \$flag{case}
)->pack( -side => 'left' );

$w{help}->attach( $w{ck_bt_case}, -balloonmsg => 'Case insensitive' );

$w{ck_bt_multiple} = $w{top_frame}->Checkbutton(
    -text     => 'm',
    -onvalue  => 'm',
    -offvalue => '',
    -variable => \$flag{single}
)->pack( -side => 'left' );

$w{help}->attach( $w{ck_bt_multiple},
    -balloonmsg => 'Multiple lines ( ^ and $ apply to each line )' );

$w{ck_bt_single} = $w{top_frame}->Checkbutton(
    -text     => 's',
    -onvalue  => 's',
    -offvalue => '',
    -variable => \$flag{multiple}
)->pack( -side => 'left' );

$w{help}
  ->attach( $w{ck_bt_single}, -balloonmsg => 'Single string ( . matches \n )' );

$w{ck_bt_ws} = $w{top_frame}->Checkbutton(
    -text     => 'x',
    -onvalue  => 'x',
    -offvalue => '',
    -variable => \$flag{ws}
)->pack( -side => 'left' );

$w{help}->attach( $w{ck_bt_ws},
    -balloonmsg =>
      'Extended regexs. (Ignore white space and comments in regex.)' );

$w{ck_bt_global} = $w{top_frame}->Checkbutton(
    -text     => 'g',
    -onvalue  => 1,
    -offvalue => 0,
    -variable => \$flag{global}
)->pack( -side => 'left' );

$w{help}->attach( $w{ck_bt_global}, -balloonmsg => 'Global search' );

$w{top_frame}->Button(
    -text    => 'Explain',
    -command => \&regexplain
)->pack( -side => 'left' );

$w{reg_error} = $w{top_frame}->Label(
    -textvariable => \$error_text,
    -borderwidth  => 2,
    -relief       => 'sunken'
)->pack( -side => 'left', -expand => 1, -fill => 'x', -padx => 4 );

## Text box for regex term. Top pane of Paned window
$w{reg_term} = $w{mw}->Scrolled(
    'TextUndo',
    -exportselection => 'true',
    -scrollbars      => 'e',
    -background      => 'white',
    -font            => $settings{font}{$OS}[0],
);

## Text box for text to apply regex to. Middle pane of Paned window
$w{reg_text} = $w{mw}->Scrolled(
    'TextUndo',
    -wrap            => 'word',
    -exportselection => 'true',
    -scrollbars      => 'e',
    -background      => 'white',
    -font            => $settings{font}{$OS}[0],
);

$w{reg_text}
  ->tagConfigure( 'highlight', -background => $settings{bg}{highlight} );
$w{reg_text}->tagLower('highlight');

## Frame to hold output widgets. Bottom pane of Paned window
$w{output_frame} = $w{mw}->Frame();

## Frame to hold match count and capture display checboxes.
$w{matches_frame} =
  $w{output_frame}->Frame( -height => 1 )->pack( -anchor => 'w' );
$w{matches_frame}->Label( -textvariable => \$matches )->pack( -side => 'left' );
$w{matches_frame}->Label( -textvariable => \$cap_disp )
  ->pack( -side => 'left' );

for ( 1 .. 9 ) {
    $w{"cap$_"} = $w{matches_frame}->Checkbutton(
        -text     => "\$$_",
        -onvalue  => 1,
        -offvalue => 0,
        -variable => \$show[$_],
    );
}

$w{rst_text} = $w{output_frame}->Scrolled(
    'ROText',
    -scrollbars => 'e',
    -background => 'white',
    -font       => $settings{font}{$OS}[0],

  )->pack(
    -side   => 'top',
    -fill   => 'both',
    -expand => 1
  );

## Fill the paned window
$w{paned_window}->add( $w{reg_term}, $w{reg_text}, $w{output_frame} );

## Reset the geometry to saved value
$w{mw}->geometry( $settings{geometry} );

## Set the sashes
for my $index ( 0, 1 ) {
    $w{mw}->update;
    $w{paned_window}->sashPlace( $index, @{ $settings{sash}[$index] } );
}

## Load the default/saved term and text
$w{reg_term}->Contents( $settings{regex} );
$w{reg_text}->Contents( $settings{text} );

## Update the fields periodically
$w{mw}->repeat( 500, \&update );

## Build the menues
$w{menu} = $w{mw}->Menu( -type => 'menubar' );
$w{mw}->configure( -menu => $w{menu} );
buildmenu();

$w{mw}->update;

$w{mw}->bind(
    '<Configure>' => sub {    # Detect geometry changes
        $settings{geometry} = $w{mw}->geometry;
        for my $index ( 0, 1 ) {
            @{ $settings{sash}[$index] } = $w{paned_window}->sashCoord($index);
        }
        save_settings();      # And save them
    }
);

## Set the fonts for the various items
$w{menu}->RefontTree( -font => $settings{font}{$OS}[1] );
$w{top_frame}->RefontTree( -font => $settings{font}{$OS}[2] );
$w{matches_frame}->RefontTree( -font => $settings{font}{$OS}[2] );

$w{mw}->protocol( 'WM_DELETE_WINDOW' =>
      sub { $w{splainpop}->destroy if defined $w{splainpop}; $w{mw}->destroy }
);

MainLoop;

sub update {    # Check term and matches periodically.
    return if $update;    # Short circuit if already in the middle of an update.
    $update = 1;

    # Some errors are runtime, not compile time, so trap STDERR
    open( STDERR, '>', ( $OS eq 'Win32' ) ? 'NULL' : '/dev/null' ) unless DEBUG;

    my $term = $w{reg_term}->Contents;
    chomp $term;

    my $flags = $flag{case} . $flag{single} . $flag{multiple} . $flag{ws};

    if ( my $whoopsie = invalid($term) ) {    # Check the regex.
        whine($whoopsie);   # Uh oh, There's a compile time regex error. Notify,
        $w{reg_text}->tagRemove( 'highlight', '1.0', 'end' )
          ;                 # remove any highlighting
        $update = 0;        # short circuit
        return;             # and bail.
    }

    $error_text = 'Regex Ok';    # Yay. No errors.
    $w{reg_error}->configure( -background => $settings{bg}{ok} );

    my $text = $w{reg_text}->Contents;
    chomp $text;
    my ( @results, $i, $cap_count );
    my @caps = $text =~ /(?$flags)$term/;    # Get a count of captures.
    my $l = defined $1;
    show_caps( my $caps = scalar @caps, $l );
    my @match_index;
    if ( $caps > 1 ) {                       # More than 1 capture in regex.
        my @allcaps;
        if ( $flag{global} ) {
            @allcaps = $text =~ /(?$flags)$term/g;    # global regex.
            while ( $text =~ /(?$flags)$term/g ) {
                push @match_index,
                  [ $-[0], ( $+[0] - $-[0] ) ];       # get match indicies
            }
        }
        else {
            @allcaps = $text =~ /(?$flags)$term/;     # single (non-global)
            push @match_index,
              [ $-[0], ( $+[0] - $-[0] ) ];           # get match indicies
        }
        $cap_count = 'Matches: ' . scalar @allcaps / $caps;
        for ( 0 .. $#allcaps ) {
            my $index = $_ % $caps;
            $i++ unless $index;
            next unless $show[ $index + 1 ];    # Only save desired captures
            push @results,
              ( $i . '($' . ( 1 + $index ) . "):\t" . $allcaps[$_] );
        }
    }
    elsif ( $flag{global} ) {                   # global regex.
        given (1) {
            when ( $show[1] and $l ) {          # has captures
                @results =
                  map { ++$i . "(\$1):\t" . $_ } $text =~ /(?$flags)$term/g;
                while ( $text =~ /(?$flags)$term/g ) {
                    push @match_index,
                      [ $-[0], ( $+[0] - $-[0] ) ];    # get match indicies
                }
            };
            when ( !$show[1] and $l ) {                # no show
                @results = map { '' } $text =~ /(?$flags)$term/g;
                while ( $text =~ /(?$flags)$term/gs ) {
                    push @match_index,
                      [ $-[0], ( $+[0] - $-[0] ) ];    # get match indicies
                }
            };
            default {                                  # no captures
                @results =
                  map { ++$i . ":\t" . $_ } $text =~ /(?$flags)$term/g;
                while ( $text =~ /(?$flags)$term/g ) {
                    push @match_index,
                      [ $-[0], ( $+[0] - $-[0] ) ];    # get match indicies
                }
            };
        }
    }
    else {
        @results = $text =~ /(?$flags)$term/;    # single term, 1 or 0 captures
        push @match_index, [ $-[0], ( $+[0] - $-[0] ) ];    # get match indicies
    }

    my $results = join "\n", @results;
    $matches = $cap_count ? $cap_count : 'Matches: ' . scalar @results;

    if ( $text ne $lasttext or $term ne $lastterm ) {

        # Force an update if text or term has changed
        adjust_highlighting( \@match_index );
        ( $lasttext, $lastterm ) = ( $text, $term );
        $update = 0;
        return;
    }
    if ( $text eq "\n" or $term eq '' ) {
        $matches = 'Matches: 0';
        $w{rst_text}->Contents("");
        $update = 0;
        return;    # Don't update if no term or text.
    }
    if ( $w{rst_text}->Contents eq $results . "\n" ) {
        $update = 0;
        return;    # Don't update if match results hasn't changed.
    }
    $w{rst_text}->Contents($results);
    adjust_highlighting( \@match_index );
    $update = 0;
}

sub adjust_highlighting {
    my $matches = shift;
    $w{reg_text}->tagRemove( 'highlight', '1.0', 'end' );

    # remove highlighting from text.
    $w{mw}->Busy;
    my ( $lineidx, $matchacc ) = ( 1, 0 );
    for my $match (@$matches)
    {    # highlight the match indicies previously captured.
        while (1) {
            my $linelen =
              length( $w{reg_text}->get( "$lineidx.0", "$lineidx.end" ) ) + 1;
            last if ( ( $matchacc + $linelen ) > $match->[0] );
            $matchacc += $linelen;
            $lineidx++;
        }
        my $offset = $match->[0] - $matchacc;
        $w{reg_text}->tagAdd( 'highlight', "$lineidx.$offset",
            "$lineidx.$offset +" . ( $match->[1] ) . 'c' );
    }
    $w{mw}->Unbusy;
}

sub invalid {    # Check to see if a regex is valid.
    my $term = shift;    # Don't bother trying to parse it,
    eval { '' =~ m/$term/; };    # let perl do it for us.
    return $@;
}

sub whine {
    my $error = shift;
    $error =~ s/ at .+?$//;                 # Massage error text a bit.
    $error =~ s/[\cM\cJ]//g;
    $error =~ s/marked by <-- HERE in .+//s;
    $error_text = 'Regex Error: ' . $error;    # And display it.
    $w{reg_error}->configure( -background => $settings{bg}{err} );
    $w{rst_text}->Contents('');
    $matches = 'Matches: 0';
    $w{reg_text}->tagRemove( 'highlight', '1.0', 'end' );
}

sub Tk::Error {                                       # Trap runtime errors.
    my ( $w, $error, @msgs ) = @_;
    whine($error)
      if $error =~ /Unicode property/;    # Report unicode property errors
    say $error if DEBUG;
    return;
}

sub show_caps {                           # Show or hide capture checkboxes
    my ( $show, $cap1 ) = @_;
    if ($cap1) {
        $cap_disp = ' --  Captures: ';
        for ( 1 .. $show ) { $w{"cap$_"}->pack( -side => 'left' ); }
        for ( $show + 1 .. 9 ) {
            $w{"cap$_"}->packForget;
            $show[$_] = 0;
        }
    }
    else {
        $cap_disp = '';
        $w{"cap$_"}->packForget for 1 .. 9;
    }
}

sub get_font {
    my $init = shift;
    $w{fd}->{'curr_font'} = $w{fd}->fontCreate( $w{fd}->fontActual($init) );
    my $font = $w{fd}->Show;
    return $font // $init;
}

sub apply_font {
    my ( $font, @widgets ) = @_;
    for (@widgets) {
        $_->RefontTree( -font => $font );
    }
}

sub buildmenu {               # Build menus
    $w{menu}->Cascade(
        -label     => 'Metachars & Assertions',
        -tearoff   => 1,
        -menuitems => [
            map { item($_) } (
                [ '\\',  '    Quote the next metacharacter' ],
                [ '^',   '    Match the beginning of a line' ],
                [ '.',   '    Match any character (except newline)' ],
                [ '$',   '    Match the end of a line' ],
                [ '|',   '    Alternation' ],
                [ '( )', '  Grouping' ],
                [ '[ ]', '  Character class' ],
                ['sep'],
                [ '\b', '   Match a word boundary' ],
                [ '\B', '   Match except at a word boundary' ],
                [ '\A', '   Match only at beginning of string' ],
                [ '\Z', '   Match only at end, or before newline at the end' ],
                [ '\z', '   Match only at end of string' ],
                [ '\G', '   Match only at pos()' ]
            )
        ]
    );
    $w{menu}->Cascade(
        -label     => 'Quantifiers',
        -tearoff   => 1,
        -menuitems => [
            map { item($_) } (
                [ '*',      '       Match 0 or more times' ],
                [ '+',      '       Match 1 or more times' ],
                [ '?',      '       Match 1 or 0 times' ],
                [ '{n}',    '     Match exactly n times' ],
                [ '{n,}',   '    Match at least n times' ],
                [ '{n,m}',  '   Match at least n but not more than m times' ],
                [ '*?',     '      Match 0 or more times, not greedily' ],
                [ '+?',     '      Match 1 or more times, not greedily' ],
                [ '??',     '      Match 0 or 1 time, not greedily' ],
                [ '{n}?',   '    Match exactly n times, not greedily' ],
                [ '{n,}?',  '   Match at least n times, not greedily' ],
                [ '{n,m}?', '  Match between n and m times, not greedily' ],
                [ '*+',   '      Match 0 or more times and give nothing back' ],
                [ '++',   '      Match 1 or more times and give nothing back' ],
                [ '?+',   '      Match 0 or 1 time and give nothing back' ],
                [ '{n}+', '    Match exactly n times and give nothing back' ],
                [ '{n,}+',  '   Match at least n times and give nothing back' ],
                [ '{n,m}+', '  Match from n to m times and give nothing back' ]
            )
        ]
    );
    $w{menu}->Cascade(
        -label     => 'Grouping',
        -tearoff   => 1,
        -menuitems => [
            map { item($_) } (
                [ '(?#text)',      '           A comment' ],
                [ '(?pimsx-imsx)', '      Enable / Disable modifier flags' ],
                [ '(?:pattern)',   '        Non-capturing cluster' ],
                [ '(?|pattern)',   '        Branch reset' ],
                [ '(?=pattern)',   '        Zero-width positive look-ahead' ],
                [ '(?!pattern)',   '        Zero-width negative look-ahead' ],
                [ '(?<=pattern)',  '       Zero-width positive look-behind' ],
                [ '(?<!pattern)',  '       Zero-width negative look-behind' ],
                [ '(?\'NAME\'pattern)', '   A named capture buffer' ],
                [ '(?<NAME>pattern)',   '   A named capture buffer' ],
                [ '\k\'NAME\'',         '           Named backreference' ],
                [ '\k<NAME>',           '           Named backreference' ]
            )
        ]
    );
    $w{menu}->Cascade(
        -label     => 'Escapes',
        -tearoff   => 1,
        -menuitems => [
            map { item($_) } (
                [ '\t', '        Tab' ],
                [ '\n', '        Newline' ],
                [ '\r', '        Return' ],
                [ '\f', '        Form feed' ],
                [ '\a', '        Alarm (bell)' ],
                [ '\e', '        Escape (think troff)' ],
                [ '\l', '        Lowercase next char (think vi)' ],
                [ '\u', '        Uppercase next char (think vi)' ],
                [ '\L', '        Lowercase till \E (think vi)' ],
                [ '\U', '        Uppercase till \E (think vi)' ],
                [ '\E', '        End case modification (think vi)' ],
                [ '\Q', '        Quote metacharacters till \E' ],
            )
        ]
    );
    $w{menu}->Cascade(
        -label     => 'Classes',
        -tearoff   => 1,
        -menuitems => [
            map { item($_) } (
                [ '\w',  '        Match a word character (alphanumeric or _)' ],
                [ '\W',  '        Match a non-"word" character' ],
                [ '\s',  '        Match a whitespace character' ],
                [ '\S',  '        Match a non-whitespace character' ],
                [ '\d',  '        Match a digit character' ],
                [ '\D',  '        Match a non-digit character' ],
                [ '\pP', '       Match P, named property (short form).' ],
                [ '\p{Prop}', '  Match named property.' ],
                [ '\PP',      '       Match non-P' ],
                [ '\P{Prop}', '  Match not named property.' ],
                [ '\X',       '        Match eXtended Unicode sequence' ],
                [ '\C',  '        Match a single C char, even under Unicode.' ],
                [ '\1',  '        Reference to a capture group' ],
                [ '\g1', '       Reference to a specific group,' ],
                [
                    '\g{-1}',
'    Negative means a previous buffer, use brackets for safer parsing.'
                ],
                [ '\g{name}', '  Named backreference' ],
                [ '\k<name>', '  Named backreference' ],
                [
                    '\K',
                    '        Keep the stuff left of \K, don\'t include in $&'
                ],
                [ '\v',       '        Vertical whitespace' ],
                [ '\V',       '        Not vertical whitespace' ],
                [ '\h',       '        Horizontal whitespace' ],
                [ '\H',       '        Not horizontal whitespace' ],
                [ '\R',       '        Linebreak' ],
                [ '\0**',     '      Octal char' ],
                [ '\x**',     '      Hex char' ],
                [ '\x{****}', '  Long hex char' ],
                [ '\c*',      '       Control char' ],
                [ '\N{name}', '  Named Unicode character' ]
            )
        ]
    );
    $w{menu}->Cascade(
        -label     => 'POSIX',
        -tearoff   => 1,
        -menuitems => [
            map { item($_) } (
                [ '[[:alpha:]]',  '   Unicode alphabetic character' ],
                [ '[[:alnum:]]',  '   Unicode alphanumeric character' ],
                [ '[[:ascii:]]',  '   ASCII character' ],
                [ '[[:blank:]]',  '   \s + vertical tab \cK' ],
                [ '[[:cntrl:]]',  '   Control character' ],
                [ '[[:digit:]]',  '   Unicode digit' ],
                [ '[[:graph:]]',  '   Any Alphanumeric or punctuation' ],
                [ '[[:lower:]]',  '   Any lowercase character' ],
                [ '[[:print:]]',  '   Any printable character' ],
                [ '[[:punct:]]',  '   Any punctuation (special) character.' ],
                [ '[[:space:]]',  '   Any space character ([[:blank:]])' ],
                [ '[[:upper:]]',  '   Any uppercase character' ],
                [ '[[:word:]]',   '    Alphabetic character or underscore' ],
                [ '[[:xdigit:]]', '  A hex digit' ]
            )
        ]
    );
    $w{menu}->Cascade(
        -label     => 'Named Properties',
        -tearoff   => 1,
        -menuitems => [
            map { item($_) } (
                [ '',          "Too many to list. See perldoc perlunicode." ],
                [ '\p{Alpha}', '         Unicode alphabetic character' ],
                [ '\p{Alnum}', '         Unicode alphanumeric character' ],
                [ '\p{Punct}', '         Punctuation' ],
                [ '\p{ASCII}', '         \x00 through \x7f' ],
                [ '\p{HexDigit}',     '      Any hex digit' ],
                [ '\p{L}',            '             Letter' ],
                [ '\p{Lu}',           '            Upper case letter' ],
                [ '\p{Ll}',           '            Lower case letter' ],
                [ '\p{P}',            '             Punctuation' ],
                [ '\p{S}',            '             Symbol' ],
                [ '\p{Sm}',           '            Math symbol' ],
                [ '\p{Latin}',        '         Is a Latin character' ],
                [ '\p{Greek}',        '         Is a Greek character' ],
                [ '\p{InBasicLatin}', '  In the Basic Latin code block' ]
            )
        ]
    );
    $w{menu}->Cascade(
        -label     => 'Options',
        -tearoff   => 0,
        -menuitems => [
            [
                Button   => 'Text Boxes Display Font',
                -command => sub {
                    my $font = get_font( $settings{font}{$OS}[0] );
                    apply_font(
                        $font,        $w{reg_term}, $w{reg_text},
                        $w{rst_text}, $w{mlabel},   $w{labelm}
                    );
                    $settings{font}{$OS}[0] =
                      $w{fd}->GetDescriptiveFontName($font);
                    save_settings();
                  }
            ],
            [
                Button   => 'Menu Drop Down Font (Monospaced Recommended)',
                -command => sub {
                    $settings{font}{$OS}[1] =
                      $w{fd}->GetDescriptiveFontName(
                        get_font( $settings{font}{$OS}[1] ) );
                    save_settings();
                    $w{menu}->RefontTree( -font => $settings{font}{$OS}[1] );
                  }
            ],
            [
                Button   => 'Options & Feedback Display Font',
                -command => sub {
                    $settings{font}{$OS}[2] =
                      $w{fd}->GetDescriptiveFontName(
                        get_font( $settings{font}{$OS}[2] ) );
                    save_settings();
                    $w{top_frame}
                      ->RefontTree( -font => $settings{font}{$OS}[2] );
                    $w{matches_frame}
                      ->RefontTree( -font => $settings{font}{$OS}[2] );
                  }
            ],
            [
                Button   => 'Match Highlight Color',
                -command => sub {
                    $settings{bg}{highlight} =
                      color_pick( $settings{bg}{highlight} );
                    $w{reg_text}->tagConfigure( 'highlight',
                        -background => $settings{bg}{highlight} );
                    save_settings();
                  }
            ],
            [
                Button   => 'Regex Ok Highlight Color',
                -command => sub {
                    $settings{bg}{ok} = color_pick( $settings{bg}{ok} );
                    save_settings();
                  }
            ],
            [
                Button   => 'Regex Error Highlight Color',
                -command => sub {
                    $settings{bg}{err} = color_pick( $settings{bg}{err} );
                    save_settings();
                  }
            ]
        ]
    );
}

sub item {    # build a menu item
    my $itemref = shift;
    my ($item) = @$itemref;
    return undef if $item eq 'sep';
    return [
        Button   => "@$itemref",
        -command => [ sub { $w{reg_term}->insert( 'insert', $_[0] ) }, $item ]
    ];
}

sub save_settings {
    DumpFile( $settings_file, %settings );
}

sub regexplain {
    my $message;

    my $term = $w{reg_term}->Contents;
    chomp $term;
    my $flags = $flag{case} . $flag{single} . $flag{multiple} . $flag{ws};
    if ( invalid($term) ) {    # Check the regex.
        $message = 'I can\'t explain this. It\'s not a valid regex.';
    }
    else {
        my $reg = eval "qr/$term/$flags";
        $message = YAPE::Regex::Explain->new($reg)->explain();
    }

    if ( defined( $w{splainpop} ) ) {
        $w{splainpop}->deiconify;
        $w{splainpop}->raise;
        $w{splainpop}->focus;
        $w{splaintext}->delete( '1.0', 'end' );
        $w{splaintext}->insert( 'end', $message );
        $w{splaintext}->see('end');  # Work-around for scrollbar
        $w{splaintext}->update;      # that doesn't show up unless
        $w{splaintext}->see('1.0');  # window is resized.
    }
    else {
        $w{splainpop} = $w{mw}->Toplevel;
        $w{splainpop}->title('Regex Explain');

        my $frame = $w{splainpop}->Frame->pack(
            -anchor => 'nw',
            -expand => 'yes',
            -fill   => 'both'
        );
        $w{splaintext} = $frame->Scrolled(
            'ROText',
            -width      => 72,
            -height     => 30,
            -background => 'white',
            -wrap       => 'word',
            -scrollbars => 'oe',
            -font       => $settings{font}{$OS}[0],
          )->pack(
            -anchor => 'nw',
            -expand => 'yes',
            -fill   => 'both',
            -padx   => 4,
            -pady   => 4
          );
        $w{splaintext}->insert( 'end', $message );
    }
    $w{splainpop}->protocol( 'WM_DELETE_WINDOW' =>
          sub { $w{splainpop}->destroy; undef $w{splainpop} } );
}

sub color_pick {
    my $init   = shift;
    my $choice = $w{mw}->chooseColor(
        -initialcolor => $init,
        -title        => "Choose color"
    );
    return $choice // $init;
}

