#! /usr/bin/env -S perl -wnl
# Run it: ./day02.sh input.txt

BEGIN {
  our $a = 0;
  our $b = "";
  our $twoCount = 0;
  our $threeCount = 0;

  # for all boxes
  our %allSubstrings = ();
}

{
  # Part A
  my @letters = split "";
  my %lettersDict = ();
  my %lettersFreqs = ();
  foreach my $c (@letters) {
    $lettersDict{$c} += 1;
  }
  @lettersFreqs{values %lettersDict} = ();
  $twoCount++ if exists $lettersFreqs{2};
  $threeCount++ if exists $lettersFreqs{3};

  # Part B
  if ($b eq "") {
    my %boxSubStrings = ();
    foreach my $i (0 .. @letters) {
      my $boxSubset = join "", grep { defined } @letters[0..$i-1,$i+1..@letters];
      $b = $boxSubset if exists $allSubstrings{$boxSubset};
      $boxSubStrings{$boxSubset} = 1;
    }
    foreach my $str (keys %boxSubStrings) {
      $allSubstrings{$str} = 1;
    }
  }
}

END {
  $a = $twoCount * $threeCount;
  print "Solving Day02A...\n$a";
  print "Solving Day02B...\n$b";
}
