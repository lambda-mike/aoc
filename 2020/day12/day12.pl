#! /usr/bin/env nix-shell
#! nix-shell -i perl
use strict;
use warnings;
use experimental "switch";

main();

sub main {
    #my $file = "sample.txt";
    my $file = "input.txt";
    open(my $in,  "<", $file) or die "Can't open $file: $!";
    my @lines = <$in>;

    # 1177
    print "Solving Day12A...\n";
    print solveA(\@lines), "\n";

    # 46530
    print "Solving Day12B...\n";
    print solveB(\@lines), "\n";
}

sub solveB {
    my $linesRef = shift;
    my %wp = (
        "x" => 10,
        "y" => 1,
    );
    my %ship = (
        "x" => 0,
        "y" => 0,
    );
    foreach (@$linesRef) {
        if (/(N|S|E|W|L|R|F)(\d+)/) {
            my $cmd = $1;
            my $n = $2;
            given ($cmd) {
                when (/N/) { $wp{"y"} += $n }
                when (/S/) { $wp{"y"} -= $n }
                when (/E/) { $wp{"x"} += $n }
                when (/W/) { $wp{"x"} -= $n }
                when (/L/) { rotateWp(\%wp, $cmd, $n) }
                when (/R/) { rotateWp(\%wp, $cmd, $n) }
                when (/F/) { forward(\%ship, \%wp, $n) }
                default    { print "ERROR: $cmd" }
            }
        }
        else {
            print "ERROR: wrong line: $_";
        }
    }
    return manhattanDist($ship{"x"}, $ship{"y"});
}

sub rotateWp {
    my ($wpRef, $cmd, $alpha) = @_;
    # alpha 270 = alpha 90 AND side toggled
    if ($alpha == 180) {
        $wpRef->{"x"} = -$wpRef->{"x"};
        $wpRef->{"y"} = -$wpRef->{"y"};
        return;
    }
    if ($alpha == 270) {
        if ($cmd eq 'L') {
            $alpha = 90;
            $cmd = 'R';
        }
        elsif ($cmd eq 'R') {
            $alpha = 90;
            $cmd = 'L';
        }
    }
    if ($alpha == 90) {
        if ($cmd eq 'L') {
            my $oldX = $wpRef->{"x"};
            $wpRef->{"x"} = -$wpRef->{"y"};
            $wpRef->{"y"} = $oldX;
        }
        elsif ($cmd eq 'R') {
            my $oldX = $wpRef->{"x"};
            $wpRef->{"x"} = $wpRef->{"y"};
            $wpRef->{"y"} = -$oldX;
        }
    }
}

# Tests
#rotateWp({"x" => 10, "y" => 1}, "L", 90*$_) foreach 0 .. 4;
#rotateWp({"x" => 10, "y" => 1}, "R", 90*$_) foreach 0 .. 4;

sub forward {
    my ($shipRef, $wpRef, $n) = @_;
    $shipRef->{"x"} += $n * $wpRef->{"x"};
    $shipRef->{"y"} += $n * $wpRef->{"y"};
}

sub solveA {
    my $linesRef = shift;
    my %ship = (
        "direction" => "E",
        "x" => 0,
        "y" => 0,
    );
    foreach (@$linesRef) {
        if (/(N|S|E|W|L|R|F)(\d+)/) {
            my $cmd = $1;
            my $n = $2;
            $cmd = $ship{"direction"} if ($cmd eq 'F');
            given ($cmd) {
                when (/N/) { $ship{"y"} += $n }
                when (/S/) { $ship{"y"} -= $n }
                when (/E/) { $ship{"x"} += $n }
                when (/W/) { $ship{"x"} -= $n }
                when (/L/) { $ship{"direction"} = rotate($ship{"direction"}, $n, $cmd) }
                when (/R/) { $ship{"direction"} = rotate($ship{"direction"}, $n, $cmd) }
                default    { print "ERROR: $1" }
            }
        }
        else {
            print "ERROR: wrong line: $_";
        }
    }
    return manhattanDist($ship{"x"}, $ship{"y"});
}

sub manhattanDist {
    my ($x, $y) = @_;
    return abs($x) + abs($y);
}

sub nextLeft {
    my $dir = shift;
    given ($dir) {
        when (/N/) { return "W" }
        when (/S/) { return "E" }
        when (/E/) { return "N" }
        when (/W/) { return "S" }
        default    { print "Wrong direction: $dir" }
    }
    return $dir;
}

sub nextRight {
    my $dir = shift;
    given ($dir) {
        when (/N/) { return "E" }
        when (/S/) { return "W" }
        when (/E/) { return "S" }
        when (/W/) { return "N" }
        default    { print "Wrong direction: $dir" }
    }
    return $dir;
}

sub angle2turns {
    my $alpha = shift;
    my $turns = 0;
    given ($alpha) {
        when (/90/)  { $turns = 1 }
        when (/180/) { $turns = 2 }
        when (/270/) { $turns = 3 }
        default      { $turns = 0 }
    }
    return $turns;
}

# Tests
#rotate({"direction" => "E", "x" => 0, "y" => 0 }, 90*$_, "L") foreach 0 .. 4;
#rotate({"direction" => "E", "x" => 0, "y" => 0 }, 90*$_, "R") foreach 0 .. 4;

sub rotate {
    my ($dir, $angle, $side) = @_;
    foreach my $i (1 .. angle2turns($angle)) {
        if ($side eq 'L') {
            $dir = nextLeft($dir);
        } 
        elsif ($side eq 'R') {
            $dir = nextRight($dir);
        } 
    }
    return $dir;
}
