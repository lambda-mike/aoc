const std = @import("std");
const File = std.fs.File;
const Reader = std.io.Reader(File, std.os.ReadError, std.fs.File.read);
const print = std.debug.print;
const AutoHashMap = std.AutoHashMap;
const parseUnsigned = std.fmt.parseUnsigned;
const assert = std.debug.assert;
const ArrayList = std.ArrayList;

const bitsPerTile: u4 = 8;
const monster = "#    ##    ##    ###";
const ParseLineError = error{ParseLineError};

const Id = u16;

// Each tile is a square with 4 borders - each has 10bits
const Border = u10;

const Side = enum {
    Top,
    Right,
    Bottom,
    Left,
};

// Tile can be "as is" (heading North), rotated 90deg (East),
// rotated 180deg (South), rotated 270deg (West), flipped horizontally or
// vertically
const Orientation = enum {
    North,
    NorthH, // Horizontal flip
    NorthV, // Vertical flip
    East,
    EastH,
    EastV,
    South,
    West,
    // NH == SV
    // NV == SH
    // EH == WV
    // EV == WH
};

const orientations = [_]Orientation{
    .North,
    .NorthH,
    .NorthV,
    .East,
    .EastH,
    .EastV,
    .South,
    .West,
};

const Borders = struct {
    top: Border,
    right: Border,
    bottom: Border,
    left: Border,
};

const Pos = struct {
    x: i8,
    y: i8,
};

const PositionsMap = AutoHashMap(Pos, Id);

// Properly placed tile with fixed orientation
const Puzzle = struct {
    id: Id,
    orientation: Orientation,
};

// null means edge
const Connection = struct {
    this: Puzzle,
    top: ?Puzzle,
    right: ?Puzzle,
    bottom: ?Puzzle,
    left: ?Puzzle,
    // Coordinates of the puzzle piece (connection)
    pos: Pos,
};

const Connections = AutoHashMap(Id, Connection);

const Tile = struct {
    const Self = @This();

    id: Id,
    north: Borders,
    northH: Borders,
    northV: Borders,
    east: Borders,
    eastH: Borders,
    eastV: Borders,
    south: Borders,
    west: Borders,

    // Inner body (without borders) of the tile; initially north orientation
    inner: [8]u8 = undefined,

    pub fn init(id: Id, original: Borders, inner: [8]u8) Tile {
        // Start with 4 North borders
        var allDirections = [_]Borders{original} ** 4;
        for (allDirections) |*dir, i| {
            if (i == 0) continue;
            const prev = allDirections[i - 1];
            dir.*.top = reverseBorder(prev.left);
            dir.*.right = prev.top;
            dir.*.bottom = reverseBorder(prev.right);
            dir.*.left = prev.bottom;
        }
        const north = allDirections[0];
        const northH = flipHorizontal(north);
        const northV = flipVertical(north);
        const east = allDirections[1];
        const eastH = flipHorizontal(east);
        const eastV = flipVertical(east);
        const south = allDirections[2];
        const west = allDirections[3];

        return Tile{
            .id = id,
            .north = north,
            .northH = northH,
            .northV = northV,
            .east = east,
            .eastH = eastH,
            .eastV = eastV,
            .south = south,
            .west = west,
            .inner = inner,
        };
    }

    pub fn getBorders(self: Self, orientation: Orientation) Borders {
        return switch (orientation) {
            .North => self.north,
            .NorthH => self.northH,
            .NorthV => self.northV,
            .East => self.east,
            .EastH => self.eastH,
            .EastV => self.eastV,
            .South => self.south,
            .West => self.west,
        };
    }

    pub fn fits(self: Self, orientation: Orientation, edge: Side, other: Tile, otherO: Orientation) bool {
        const selfTile = self.getBorders(orientation);
        const otherTile = other.getBorders(otherO);
        return switch (edge) {
            .Top => selfTile.top == otherTile.bottom,
            .Right => selfTile.right == otherTile.left,
            .Bottom => selfTile.bottom == otherTile.top,
            .Left => selfTile.left == otherTile.right,
        };
    }

    fn innerFlipVertical(self: *Self) void {
        var rows = &self.inner;
        // Iterate through half of the 8-element array and swap opposite items
        // in a symmetrical manner 0 <-> 7, 1 <-> 6, 2 <-> 5, 3 <-> 4
        for ([_]u2{ 0, 1, 2, 3 }) |i| {
            var tmp = rows[i];
            // 8 rows, -1 is a last row - i is a count from the end of the array
            // opposite to i in 8 element array
            var opposite = 8 - 1 - @intCast(u3, i);
            rows[i] = rows[opposite];
            rows[opposite] = tmp;
        }
    }

    test "innerFlipVertical" {
        const inner = [_]u8{ 0, 1, 2, 3, 4, 5, 6, 7 };
        const id: Id = 1951;
        const input = comptime Borders{
            .top = 0b1011000110,
            .right = 0b0111110010,
            .bottom = 0b1000110100,
            .left = 0b1101001001,
        };
        var tile = comptime Tile.init(id, input, inner);
        var expected = [_]u8{ 7, 6, 5, 4, 3, 2, 1, 0 };
        tile.innerFlipVertical();
        try std.testing.expectEqual(expected, tile.inner);
    }

    fn innerFlipHorizontal(self: *Self) void {
        for (self.inner) |*bits| {
            bits.* = reverseBits(u8, bits.*);
        }
    }

    test "innerFlipHorizontal" {
        const inner = [_]u8{ 0b11000110, 0b11110010, 0b00110100, 0b01001001, 0b11001101, 0b01011111, 0b01001111, 0b01001011 };
        const id: Id = 1951;
        const input = comptime Borders{
            .top = 0b1011000110,
            .right = 0b0111110010,
            .bottom = 0b1000110100,
            .left = 0b1101001001,
        };
        var tile = comptime Tile.init(id, input, inner);
        var expected = [_]u8{ 0b01100011, 0b01001111, 0b00101100, 0b10010010, 0b10110011, 0b11111010, 0b11110010, 0b11010010 };
        tile.innerFlipHorizontal();
        try std.testing.expectEqual(expected, tile.inner);
    }

    fn innerTranspose(self: *Self) void {
        // array of rows (result of transposition of inner)
        var transposed = [_]u8{0} ** 8;
        inline for ([_]u8{ 7, 6, 5, 4, 3, 2, 1, 0 }) |bit| {
            // nth bit counting from right
            const mask: u8 = 1 << bit;
            for (self.inner) |n, i| {
                // Extract bit from n using mask, shift to the first position,
                // then shift left to the (7 - i)-th position and assign to transposed
                transposed[bit] |= (mask & n) >> bit << @intCast(u3, 7 - i);
            }
        }
        self.inner = transposed;
    }

    test "innerTranspose" {
        const inner = [_]u8{
            0b11000110,
            0b11110010,
            0b00110100,
            0b01001001,
            0b11001101,
            0b01011111,
            0b01001111,
            0b01001011,
        };
        const id: Id = 1951;
        const input = comptime Borders{
            .top = 0b1011000110,
            .right = 0b0111110010,
            .bottom = 0b1000110100,
            .left = 0b1101001001,
        };
        var tile = comptime Tile.init(id, input, inner);
        var expected = [_]u8{
            0b00011111,
            0b11000111,
            0b10101110,
            0b00011111,
            0b01100100,
            0b01100000,
            0b11011111,
            0b11001000,
        };
        tile.innerTranspose();
        try std.testing.expectEqual(expected, tile.inner);
    }

    pub fn orientateInner(self: *Self, orientation: Orientation) void {
        switch (orientation) {
            .North => {},
            .NorthH => self.innerFlipHorizontal(),
            .NorthV => self.innerFlipVertical(),
            .East => self.innerTranspose(),
            .EastH => {
                self.innerTranspose();
                self.innerFlipHorizontal();
            },
            .EastV => {
                self.innerTranspose();
                self.innerFlipVertical();
            },
            .South => {
                self.innerTranspose();
                self.innerTranspose();
            },
            .West => {
                self.innerTranspose();
                self.innerTranspose();
                self.innerTranspose();
            },
        }
    }

    const innerTest = [_]u8{
        0b11000110,
        0b11110010,
        0b00110100,
        0b01001001,
        0b11001101,
        0b01011111,
        0b01001111,
        0b01001011,
    };
    const idTest: Id = 1951;
    const inputTest = Borders{
        .top = 0b1011000110,
        .right = 0b0111110010,
        .bottom = 0b1000110100,
        .left = 0b1101001001,
    };
    test "orientateInner.North" {
        var tile = comptime Tile.init(idTest, inputTest, innerTest);
        tile.orientateInner(.North);
        try std.testing.expectEqual(innerTest, tile.inner);
    }

    test "orientateInner2473" {
        const inner2473 = [_]u8{
            0b10001101,
            0b11111110,
            0b11101001,
            0b11111111,
            0b10001010,
            0b11111010,
            0b01100100,
            0b00101100,
        };
        const expected2473 = [_]u8{
            0b11111100,
            0b01110110,
            0b01110111,
            0b01010100,
            0b11111101,
            0b11010011,
            0b01011100,
            0b10110000,
        };
        var tile2473 = comptime Tile.init(idTest, inputTest, inner2473);
        tile2473.orientateInner(.EastV);
        try std.testing.expectEqual(expected2473, tile2473.inner);
    }

    test "orientateInner.NorthH" {
        var tile = comptime Tile.init(idTest, inputTest, innerTest);
        tile.orientateInner(.NorthH);
        var expected = [_]u8{
            0b01100011,
            0b01001111,
            0b00101100,
            0b10010010,
            0b10110011,
            0b11111010,
            0b11110010,
            0b11010010,
        };
        try std.testing.expectEqual(expected, tile.inner);
    }

    test "orientateInner.NorthV" {
        var tile = comptime Tile.init(idTest, inputTest, innerTest);
        tile.orientateInner(.NorthV);
        const expected = [_]u8{
            0b01001011,
            0b01001111,
            0b01011111,
            0b11001101,
            0b01001001,
            0b00110100,
            0b11110010,
            0b11000110,
        };
        try std.testing.expectEqual(expected, tile.inner);
    }

    test "orientateInner.East" {
        var tile = comptime Tile.init(idTest, inputTest, innerTest);
        tile.orientateInner(.East);
        const expected = [_]u8{
            0b00011111,
            0b11000111,
            0b10101110,
            0b00011111,
            0b01100100,
            0b01100000,
            0b11011111,
            0b11001000,
        };
        try std.testing.expectEqual(expected, tile.inner);
    }

    test "orientateInner.EastH" {
        var tile = comptime Tile.init(idTest, inputTest, innerTest);
        tile.orientateInner(.EastH);
        const expected = [_]u8{
            0b11111000,
            0b11100011,
            0b01110101,
            0b11111000,
            0b00100110,
            0b00000110,
            0b11111011,
            0b00010011,
        };
        try std.testing.expectEqual(expected, tile.inner);
    }

    test "orientateInner.EastV" {
        var tile = comptime Tile.init(idTest, inputTest, innerTest);
        tile.orientateInner(.EastV);
        const expected = [_]u8{
            0b11001000,
            0b11011111,
            0b01100000,
            0b01100100,
            0b00011111,
            0b10101110,
            0b11000111,
            0b00011111,
        };
        try std.testing.expectEqual(expected, tile.inner);
    }

    test "orientateInner.South" {
        var tile = comptime Tile.init(idTest, inputTest, innerTest);
        tile.orientateInner(.South);
        const expected = [_]u8{
            0b11010010,
            0b11110010,
            0b11111010,
            0b10110011,
            0b10010010,
            0b00101100,
            0b01001111,
            0b01100011,
        };
        try std.testing.expectEqual(expected, tile.inner);
    }

    test "orientateInner.West" {
        var tile = comptime Tile.init(idTest, inputTest, innerTest);
        tile.orientateInner(.West);
        const expected = [_]u8{
            0b00010011,
            0b11111011,
            0b00000110,
            0b00100110,
            0b11111000,
            0b01110101,
            0b11100011,
            0b11111000,
        };
        try std.testing.expectEqual(expected, tile.inner);
    }
};

test "Tile" {
    const inner: [8]u8 = undefined;
    const id: Id = 1951;
    const input = comptime Borders{
        .top = 0b1011000110,
        .right = 0b0111110010,
        .bottom = 0b1000110100,
        .left = 0b1101001001,
    };
    const tile = comptime Tile.init(id, input, inner);

    try std.testing.expectEqual(input.top, tile.north.top);
    try std.testing.expectEqual(input.right, tile.north.right);
    try std.testing.expectEqual(input.bottom, tile.north.bottom);
    try std.testing.expectEqual(input.left, tile.north.left);

    try std.testing.expectEqual(0b0110001101, tile.northH.top);
    try std.testing.expectEqual(input.left, tile.northH.right);
    try std.testing.expectEqual(0b0010110001, tile.northH.bottom);
    try std.testing.expectEqual(input.right, tile.northH.left);

    try std.testing.expectEqual(input.bottom, tile.northV.top);
    try std.testing.expectEqual(0b0100111110, tile.northV.right);
    try std.testing.expectEqual(input.top, tile.northV.bottom);
    try std.testing.expectEqual(0b1001001011, tile.northV.left);

    try std.testing.expectEqual(0b1001001011, tile.east.top);
    try std.testing.expectEqual(input.top, tile.east.right);
    try std.testing.expectEqual(0b0100111110, tile.east.bottom);
    try std.testing.expectEqual(input.bottom, tile.east.left);

    try std.testing.expectEqual(0b1101001001, tile.eastH.top);
    try std.testing.expectEqual(tile.east.left, tile.eastH.right);
    try std.testing.expectEqual(0b0111110010, tile.eastH.bottom);
    try std.testing.expectEqual(tile.east.right, tile.eastH.left);

    try std.testing.expectEqual(0b0010110001, tile.south.top);
    try std.testing.expectEqual(0b1001001011, tile.south.right);
    try std.testing.expectEqual(0b0110001101, tile.south.bottom);
    try std.testing.expectEqual(0b0100111110, tile.south.left);

    try std.testing.expectEqual(input.right, tile.west.top);
    try std.testing.expectEqual(0b0010110001, tile.west.right);
    try std.testing.expectEqual(input.left, tile.west.bottom);
    try std.testing.expectEqual(0b0110001101, tile.west.left);
}

test "Tile::fits" {
    const inner: [8]u8 = undefined;
    const inputA = comptime Borders{
        .top = 0b1011000110,
        .right = 0b0111110010,
        .bottom = 0b1000110100,
        .left = 0b1101001001,
    };
    const tileA = comptime Tile.init(1951, inputA, inner);

    const inputB = comptime Borders{
        .top = 0b0001010101,
        .right = 0b1001000000,
        .bottom = 0b1011000110,
        .left = 0b0100001111,
    };
    const tileB = comptime Tile.init(2729, inputB, inner);

    const inputC = comptime Borders{
        .top = 0b1110110100,
        .right = 0b0011101010,
        .bottom = 0b0011010010,
        .left = 0b1001000000,
    };
    const tileC = comptime Tile.init(1427, inputC, inner);
    const inputD = comptime Borders{
        .top = 0b1000011110,
        .right = 0b0001110100,
        .bottom = 0b0011101010,
        .left = 0b1111000110,
    };
    const tileD = comptime Tile.init(2473, inputD, inner);

    try std.testing.expect(tileA.fits(.NorthV, .Bottom, tileB, .NorthV));
    try std.testing.expect(tileB.fits(.NorthV, .Top, tileA, .NorthV));
    // All other orientations in Bottom dir
    try std.testing.expect(!tileA.fits(.NorthV, .Bottom, tileB, .North));
    try std.testing.expect(!tileA.fits(.NorthV, .Bottom, tileB, .NorthH));
    try std.testing.expect(!tileA.fits(.NorthV, .Bottom, tileB, .South));
    try std.testing.expect(!tileA.fits(.NorthV, .Bottom, tileB, .East));
    try std.testing.expect(!tileA.fits(.NorthV, .Bottom, tileB, .EastH));
    try std.testing.expect(!tileA.fits(.NorthV, .Bottom, tileB, .EastV));
    try std.testing.expect(!tileA.fits(.NorthV, .Bottom, tileB, .West));
    // Other directions
    try std.testing.expect(!tileA.fits(.NorthV, .Left, tileB, .NorthV));
    try std.testing.expect(!tileA.fits(.NorthV, .Right, tileB, .NorthV));
    try std.testing.expect(!tileA.fits(.NorthV, .Top, tileB, .NorthV));
    // Far distant tile
    try std.testing.expect(!tileA.fits(.NorthV, .Top, tileD, .NorthV));
    try std.testing.expect(!tileA.fits(.NorthV, .Top, tileD, .South));

    try std.testing.expect(tileC.fits(.NorthV, .Right, tileD, .EastV));
    try std.testing.expect(tileD.fits(.EastV, .Left, tileC, .NorthV));
}

const TilesMap = AutoHashMap(Id, Tile);

fn flipHorizontal(tile: Borders) Borders {
    return Borders{
        .top = reverseBorder(tile.top),
        .right = tile.left,
        .bottom = reverseBorder(tile.bottom),
        .left = tile.right,
    };
}

fn flipVertical(tile: Borders) Borders {
    return Borders{
        .top = tile.bottom,
        .right = reverseBorder(tile.right),
        .bottom = tile.top,
        .left = reverseBorder(tile.left),
    };
}

fn reverseBits(comptime T: type, b: T) T {
    var bits = b;
    var result: T = 0;
    const n = @bitSizeOf(T) - 1;
    for ([_]T{0} ** n) |_| {
        result |= 1 & bits;
        bits >>= 1;
        result <<= 1;
    }
    // last bit
    result |= 1 & bits;
    return result;
}

fn reverseBorder(b: Border) Border {
    return reverseBits(Border, b);
}

test "reverseBorder" {
    comptime var border: u10 = 0b0000000000;
    try std.testing.expectEqual(border, comptime reverseBorder(border));
    border = 0b0000000001;
    try std.testing.expectEqual(0b1000000000, comptime reverseBorder(border));
    border = 0b1000000001;
    try std.testing.expectEqual(0b1000000001, comptime reverseBorder(border));
    border = 0b0000000101;
    try std.testing.expectEqual(0b1010000000, comptime reverseBorder(border));
    border = 0b0011010010;
    try std.testing.expectEqual(0b0100101100, comptime reverseBorder(border));
}

fn parseLine(line: []const u8) ParseLineError!Border {
    var result: Border = 0;
    for (line) |c| {
        result <<= 1;
        switch (c) {
            '#' => {
                result |= 1;
            },
            // '.' is bit 0 - do nothing;
            // we have already done bit shift left before switch above
            '.' => {},
            else => return error.ParseLineError,
        }
    }
    return result;
}

test "parseLine" {
    try std.testing.expectError(error.ParseLineError, parseLine("astrst"));

    const cases = .{
        .{ "..##.#..#.", 0b0011010010 },
        .{ "##.##.###.", 0b1101101110 },
        .{ "..#....#..", 0b0010000100 },
    };

    inline for (cases) |tuple| {
        const line = try parseLine(tuple.@"0");
        const expected: Border = tuple.@"1";
        try std.testing.expectEqual(expected, line);
    }
}

// Read file line by line, parse tiles and populate tiles hash map
fn parseInput(reader: Reader, tiles: *TilesMap) !void {
    var id: Id = 0;
    var n: ?Border = null;
    var e: Border = 0;
    var s: ?Border = null;
    var w: Border = 0;
    // 11 = 10 chars + \n
    var buffer: [11]u8 = undefined;
    var inner: [8]u8 = [_]u8{0} ** 8;
    var innerIndex: u8 = 8;
    while (try reader.readUntilDelimiterOrEof(&buffer, "\n"[0])) |contents| {
        // End of tile - save & reset
        if (contents.len == 0) {
            const borders = Borders{
                .top = n.?,
                .right = e,
                .bottom = s.?,
                .left = w,
            };
            try tiles.*.put(id, Tile.init(id, borders, inner));
            id = 0;
            n = null;
            e = 0;
            s = null;
            w = 0;
            innerIndex = 8;
            continue;
        }
        // Every line is supposed to be non empty
        switch (contents[0]) {
            'T' => {
                // All tile ids are 4 digits in the same place
                id = try parseUnsigned(Id, buffer[5..9], 10);
            },
            '.', '#' => {
                const line = try parseLine(contents);
                // Ignore the first line (border and the last one)
                if (n != null and innerIndex > 0) {
                    // We are parsing line by line from the top, so the top
                    // line will correspond to 7 and bottom line to 0
                    inner[innerIndex - 1] = @truncate(u8, line >> 1);
                    innerIndex -= 1;
                }
                if (n == null) {
                    n = line;
                }
                s = line;
                // both east and west start as 0
                // bit shift left east and west before processing every line
                w <<= 1;
                // 9 is the last bit in the line from right, first from left
                w |= (line >> 9);
                e <<= 1;
                e |= (1 & line);
            },
            else => {
                print("Unrecognized character: {}\n", .{contents[0]});
                unreachable;
            },
        }
    }
}

fn isCorner(t: u32, r: u32, b: u32, l: u32) bool {
    if (t == 0 and r > 0 and b > 0 and l == 0) return true;
    if (t == 0 and r == 0 and b > 0 and l > 0) return true;
    if (t > 0 and r == 0 and b == 0 and l > 0) return true;
    if (t > 0 and r > 0 and b == 0 and l == 0) return true;
    return false;
}

fn countZeroes(t: u32, r: u32, b: u32, l: u32) u8 {
    var result: u8 = 4;
    if (t > 0) result -= 1;
    if (r > 0) result -= 1;
    if (b > 0) result -= 1;
    if (l > 0) result -= 1;
    return result;
}

fn solveA(tiles: TilesMap) u64 {
    var iterator = tiles.iterator();
    var result: u64 = 1;
    while (iterator.next()) |entry| {
        const id = entry.key_ptr.*;
        const tile = entry.value_ptr.*;
        // We can only check one orientation of the original tile,
        // because no matter how we flip/rotate it, its neighbours will match
        // and will be flipped/rotates (transformed) further themselves accordingly
        const orientation = .North;
        var nTop: u32 = 0;
        var nRight: u32 = 0;
        var nBottom: u32 = 0;
        var nLeft: u32 = 0;
        var inner = tiles.iterator();
        while (inner.next()) |other| {
            if (other.key_ptr.* == id) {
                continue;
            }
            const otherTile = other.value_ptr.*;
            for (orientations) |otherO| {
                nTop += if (tile.fits(orientation, .Top, otherTile, otherO)) 1 else 0;
                nRight += if (tile.fits(orientation, .Right, otherTile, otherO)) 1 else 0;
                nBottom += if (tile.fits(orientation, .Bottom, otherTile, otherO)) 1 else 0;
                nLeft += if (tile.fits(orientation, .Left, otherTile, otherO)) 1 else 0;
            }
        }
        if (isCorner(nTop, nRight, nBottom, nLeft)) {
            result *= id;
        }
    }
    return result;
}

fn printOrientation(orientation: Orientation) []const u8 {
    return switch (orientation) {
        .North => "N",
        .NorthH => "NH",
        .NorthV => "NV",
        .East => "E",
        .EastH => "EH",
        .EastV => "EV",
        .South => "S",
        .West => "W",
    };
}

fn populateConnections(tiles: TilesMap, connections: *Connections) !void {
    var iterator = tiles.iterator();
    while (iterator.next()) |entry| {
        const id = entry.key_ptr.*;
        const tile = entry.value_ptr.*;
        var inner = tiles.iterator();
        var nTop: u32 = 0;
        var nRight: u32 = 0;
        var nBottom: u32 = 0;
        var nLeft: u32 = 0;
        var top: ?Puzzle = null;
        var right: ?Puzzle = null;
        var bottom: ?Puzzle = null;
        var left: ?Puzzle = null;
        // We can only check one orientation of the original tile,
        // because no matter how we flip/rotate it, its neighbours will match
        // and will be flipped/rotates (transformed) further themselves accordingly
        const orientation = .North;
        while (inner.next()) |other| {
            if (other.key_ptr.* == id) {
                continue;
            }
            const otherTile = other.value_ptr.*;
            for (orientations) |otherO| {
                if (tile.fits(orientation, .Top, otherTile, otherO)) {
                    nTop += 1;
                    top = Puzzle{ .id = other.key_ptr.*, .orientation = otherO };
                }
                if (tile.fits(orientation, .Right, otherTile, otherO)) {
                    nRight += 1;
                    right = Puzzle{ .id = other.key_ptr.*, .orientation = otherO };
                }
                if (tile.fits(orientation, .Bottom, otherTile, otherO)) {
                    nBottom += 1;
                    bottom = Puzzle{ .id = other.key_ptr.*, .orientation = otherO };
                }
                if (tile.fits(orientation, .Left, otherTile, otherO)) {
                    nLeft += 1;
                    left = Puzzle{ .id = other.key_ptr.*, .orientation = otherO };
                }
            }
        }
        if (countZeroes(nTop, nRight, nBottom, nLeft) <= 2) {
            const connection = Connection{
                .this = Puzzle{
                    .id = id,
                    .orientation = orientation,
                },
                .top = top,
                .right = right,
                .bottom = bottom,
                .left = left,
                .pos = Pos{
                    // Position will be set correctly later
                    .x = 0,
                    .y = 0,
                },
            };
            // If memory allocation fails, panic!
            try connections.put(id, connection);
        }
    }
}

// puzzleOrientation - orientation of the puzzle connected to the source, when
// it was heading North; now sourceOrientation was updated, so we need to adjust
// puzzleOrientation properly
fn composeOrientations(source: Orientation, puzzle: Orientation) Orientation {
    return switch (source) {
        .North => puzzle,
        .NorthH => switch (puzzle) {
            .North => .NorthH,
            .NorthH => .North,
            .NorthV => .South,
            .East => .EastH,
            .EastH => .East,
            .EastV => .West,
            .South => .NorthV,
            .West => .EastV,
        },
        .NorthV => switch (puzzle) {
            .North => .NorthV,
            .NorthH => .South,
            .NorthV => .North,
            .East => .EastV,
            .EastH => .West,
            .EastV => .East,
            .South => .NorthH,
            .West => .EastH,
        },
        .East => switch (puzzle) {
            .North => .East,
            .NorthH => .EastV,
            .NorthV => .EastH,
            .East => .South,
            .EastH => .NorthH,
            .EastV => .NorthV,
            .South => .West,
            .West => .North,
        },
        .EastH => switch (puzzle) {
            .North => .EastH,
            .NorthH => .West,
            .NorthV => .East,
            .East => .NorthV,
            .EastH => .North,
            .EastV => .South,
            .South => .EastV,
            .West => .NorthH,
        },
        .EastV => switch (puzzle) {
            .North => .EastV,
            .NorthH => .East,
            .NorthV => .West,
            .East => .NorthH,
            .EastH => .South,
            .EastV => .North,
            .South => .EastH,
            .West => .NorthV,
        },
        .South => switch (puzzle) {
            .North => .South,
            .NorthH => .NorthV,
            .NorthV => .NorthH,
            .East => .West,
            .EastH => .EastV,
            .EastV => .EastH,
            .South => .North,
            .West => .East,
        },
        .West => switch (puzzle) {
            .North => .West,
            .NorthH => .EastH,
            .NorthV => .EastV,
            .East => .North,
            .EastH => .NorthV,
            .EastV => .NorthH,
            .South => .East,
            .West => .South,
        },
    };
}

fn orientatePuzzle(connections: *Connections, puzzle: *Puzzle, parentOrientation: Orientation) !Id {
    var conn = connections.get(puzzle.*.id).?;
    // Set correct orientation for original connection;
    // it is always North initially
    conn.this.orientation = composeOrientations(parentOrientation, puzzle.*.orientation);
    puzzle.*.orientation = conn.this.orientation;
    try connections.put(puzzle.*.id, conn);
    return puzzle.*.id;
}

fn fixDirections(conn: *Connection) void {
    var puzzleTop = conn.*.top;
    var puzzleRight = conn.*.right;
    var puzzleBottom = conn.*.bottom;
    var puzzleLeft = conn.*.left;
    switch (conn.*.this.orientation) {
        .North => {},
        .NorthH => {
            conn.*.left = puzzleRight;
            conn.*.right = puzzleLeft;
        },
        .NorthV => {
            conn.*.bottom = puzzleTop;
            conn.*.top = puzzleBottom;
        },
        .East => {
            conn.*.right = puzzleTop;
            conn.*.bottom = puzzleRight;
            conn.*.left = puzzleBottom;
            conn.*.top = puzzleLeft;
        },
        .EastH => {
            conn.*.right = puzzleBottom;
            conn.*.bottom = puzzleRight;
            conn.*.left = puzzleTop;
            conn.*.top = puzzleLeft;
        },
        .EastV => {
            conn.*.right = puzzleTop;
            conn.*.bottom = puzzleLeft;
            conn.*.left = puzzleBottom;
            conn.*.top = puzzleRight;
        },
        .South => {
            conn.*.top = puzzleBottom;
            conn.*.bottom = puzzleTop;
            conn.*.left = puzzleRight;
            conn.*.right = puzzleLeft;
        },
        .West => {
            conn.*.left = puzzleTop;
            conn.*.top = puzzleRight;
            conn.*.right = puzzleBottom;
            conn.*.bottom = puzzleLeft;
        },
    }
}

fn setPositions(connections: *Connections, this: *Connection, farLeft: *Pos) !void {
    if (this.top != null) {
        var id = this.top.?.id;
        var connection = connections.get(id).?;
        connection.pos = Pos{ .x = this.pos.x, .y = this.pos.y + 1 };
        try connections.put(id, connection);
        farLeft.*.x = std.math.min(farLeft.*.x, connection.pos.x);
        farLeft.*.y = std.math.min(farLeft.*.y, connection.pos.y);
    }
    if (this.right != null) {
        var id = this.right.?.id;
        var connection = connections.get(id).?;
        connection.pos = Pos{ .x = this.pos.x + 1, .y = this.pos.y };
        try connections.put(id, connection);
        farLeft.*.x = std.math.min(farLeft.*.x, connection.pos.x);
        farLeft.*.y = std.math.min(farLeft.*.y, connection.pos.y);
    }
    if (this.bottom != null) {
        var id = this.bottom.?.id;
        var connection = connections.get(id).?;
        connection.pos = Pos{ .x = this.pos.x, .y = this.pos.y - 1 };
        try connections.put(id, connection);
        farLeft.*.x = std.math.min(farLeft.*.x, connection.pos.x);
        farLeft.*.y = std.math.min(farLeft.*.y, connection.pos.y);
    }
    if (this.left != null) {
        var id = this.left.?.id;
        var connection = connections.get(id).?;
        connection.pos = Pos{ .x = this.pos.x - 1, .y = this.pos.y };
        try connections.put(id, connection);
        farLeft.*.x = std.math.min(farLeft.*.x, connection.pos.x);
        farLeft.*.y = std.math.min(farLeft.*.y, connection.pos.y);
    }
}

fn translatePositions(connections: *Connections, puzzleMap: *PositionsMap, farLeft: Pos) !void {
    var it = connections.iterator();
    while (it.next()) |*conn| {
        const pos = Pos{
            .x = conn.value_ptr.*.pos.x + try std.math.absInt(farLeft.x),
            .y = conn.value_ptr.*.pos.y + try std.math.absInt(farLeft.y),
        };
        conn.value_ptr.*.pos = pos;
        try puzzleMap.put(pos, conn.value_ptr.this.id);
    }
}

fn prettyPrintPuzzle(n: u8, puzzleMap: PositionsMap, tiles: TilesMap) void {
    var y: u8 = n;
    while (y > 0) : (y -= 1) {
        var row: u8 = 8;
        while (row > 0) : (row -= 1) {
            var x: u8 = 0;
            print("{}: ", .{row});
            while (x < n) : (x += 1) {
                const id = puzzleMap.get(Pos{ .x = @intCast(i8, x), .y = @intCast(i8, y - 1) }).?;
                const bits = tiles.get(id).?.inner[row - 1];
                // print("{b:0>8} ", .{bits});
                print("{b:0>8}", .{bits});
            }
            print("\n", .{});
        }
        print("\n", .{});
    }
}

fn orientatePuzzles(tiles: *TilesMap, connections: *Connections, puzzleMap: *PositionsMap, allocator: std.mem.Allocator) !void {
    const IdSet = AutoHashMap(Id, void);
    const IdQueue = ArrayList(Id);
    // already visited and finished tiles
    var visited = IdSet.init(allocator);
    defer visited.clearAndFree();
    // queue of tiles to process next
    var connectionIdsQueue = IdQueue.init(allocator);
    defer connectionIdsQueue.clearAndFree();
    // Keep track of far left X, Y to translate later
    var farLeft = Pos{ .x = 0, .y = 0 };

    var connectionsIt = connections.*.iterator();
    var genesisConnection = connectionsIt.next().?;
    var genesisConnectionId = genesisConnection.key_ptr.*;
    // We decide that first connesction is always correctly orientated;
    // all following connections need to be adjusted relatively
    try connectionIdsQueue.append(genesisConnectionId);
    var nextConnId = connectionIdsQueue.popOrNull();
    while (nextConnId != null) : ({
        try visited.put(nextConnId.?, {});
        nextConnId = connectionIdsQueue.popOrNull();
    }) {
        // More than one tile points at any given tile;
        // Thus do not orientate tiles again, when you encounter them
        if (visited.contains(nextConnId.?)) {
            continue;
        }
        // We assume that any connection we get from the connQueue is properly
        // orientated and correct - we only need to orientatet its neighbours
        var connection = connections.get(nextConnId.?).?;
        if (connection.top != null) {
            var puzzle = connection.top.?;
            const puzzleId = try orientatePuzzle(
                connections,
                &puzzle,
                connection.this.orientation,
            );
            connection.top.?.orientation = puzzle.orientation;
            try connectionIdsQueue.append(puzzleId);
        }
        if (connection.right != null) {
            var puzzle = connection.right.?;
            const puzzleId = try orientatePuzzle(
                connections,
                &puzzle,
                connection.this.orientation,
            );
            connection.right.?.orientation = puzzle.orientation;
            try connectionIdsQueue.append(puzzleId);
        }
        if (connection.bottom != null) {
            var puzzle = connection.bottom.?;
            const puzzleId = try orientatePuzzle(
                connections,
                &puzzle,
                connection.this.orientation,
            );
            connection.bottom.?.orientation = puzzle.orientation;
            try connectionIdsQueue.append(puzzleId);
        }
        if (connection.left != null) {
            var puzzle = connection.left.?;
            const puzzleId = try orientatePuzzle(
                connections,
                &puzzle,
                connection.this.orientation,
            );
            connection.left.?.orientation = puzzle.orientation;
            try connectionIdsQueue.append(puzzleId);
        }
        fixDirections(&connection);
        try setPositions(connections, &connection, &farLeft);
        try connections.put(connection.this.id, connection);
    }
    try translatePositions(connections, puzzleMap, farLeft);
    var it = connections.iterator();
    while (it.next()) |conn| {
        var tile = tiles.get(conn.value_ptr.*.this.id).?;
        tile.orientateInner(conn.value_ptr.*.this.orientation);
        try tiles.put(tile.id, tile);
    }
}

fn countHashes(tiles: TilesMap) u64 {
    var result: u64 = 0;
    var it = tiles.iterator();
    while (it.next()) |tile| {
        for (tile.value_ptr.*.inner) |row| {
            var n = row;
            while (n > 0) : (n >>= 1) {
                result += (n & 1);
            }
        }
    }
    return result;
}

fn extractMonsterSample(tiles: TilesMap, puzzleMap: PositionsMap, x: u8, y: u8) u20 {
    const yTile = y / bitsPerTile;
    const yRow = y % bitsPerTile;
    var result: u20 = 0;
    // from 0 to 20
    var i: u5 = 0;
    while (i < monster.len) : (i += 1) {
        const xTile = (x + i) / bitsPerTile;
        // n-th bit from right
        const xBit: u3 = @intCast(u3, bitsPerTile - @intCast(u4, (x + i) % bitsPerTile) - 1);
        // We know x and y is always smaller than i8 max
        const tileId = puzzleMap.get(Pos{ .x = @intCast(i8, xTile), .y = @intCast(i8, yTile) }).?;
        const tile = tiles.get(tileId).?;
        const tileRow = tile.inner[yRow];
        // 0 or 1
        const bit: u20 = (tileRow >> xBit) & 1;
        // Bit inside monster from right; We know monster len is u5
        const monsterBit: u5 = @intCast(u5, monster.len) - i - 1;
        result |= bit << monsterBit;
    }
    return result;
}

// n is length of the square side; we assume all positions are inside the map
// and translated correctly, so it always starts in 0,0
fn puzzleMapFlipHorizontal(n: u8, puzzleMap: *PositionsMap) !void {
    const end: u8 = n / 2;
    var y: u8 = 0;
    while (y < n) : (y += 1) {
        var x: u8 = 0;
        while (x < end) : (x += 1) {
            const pos = Pos{ .x = @intCast(i8, x), .y = @intCast(i8, y) };
            const sourceId = puzzleMap.get(pos).?;
            const oppositePos = Pos{ .x = @intCast(i8, n - x - 1), .y = @intCast(i8, y) };
            const oppositeId = puzzleMap.get(oppositePos).?;
            // swap them
            try puzzleMap.put(pos, oppositeId);
            try puzzleMap.put(oppositePos, sourceId);
        }
    }
}

// n is length of the square side; we assume all positions are inside the map
// and translated correctly, so it always starts in 0,0
fn puzzleMapFlipVertical(n: u8, puzzleMap: *PositionsMap) !void {
    const end: u8 = n / 2;
    var x: u8 = 0;
    while (x < n) : (x += 1) {
        var y: u8 = 0;
        while (y < end) : (y += 1) {
            const pos = Pos{ .x = @intCast(i8, x), .y = @intCast(i8, y) };
            const sourceId = puzzleMap.get(pos).?;
            const oppositePos = Pos{ .x = @intCast(i8, x), .y = @intCast(i8, n - y - 1) };
            const oppositeId = puzzleMap.get(oppositePos).?;
            // swap them
            try puzzleMap.put(pos, oppositeId);
            try puzzleMap.put(oppositePos, sourceId);
        }
    }
}

// n is length of the square side; we assume all positions are inside the map
// and translated correctly, so it always starts in 0,0
fn puzzleMapRotateClockwise(n: u8, puzzleMap: *PositionsMap) !void {
    var source: PositionsMap = try puzzleMap.clone();
    defer source.clearAndFree();
    var y: i8 = 0;
    while (y < n) : (y += 1) {
        var x: i8 = 0;
        while (x < n) : (x += 1) {
            const targetPos = Pos{ .x = x, .y = y };
            const sourcePos = Pos{ .x = @intCast(i8, n) - y - 1, .y = x };
            const id = source.get(sourcePos).?;
            try puzzleMap.put(targetPos, id);
        }
    }
}

fn puzzleMapOrientate(n: u8, puzzleMap: *PositionsMap, orientation: Orientation) !void {
    switch (orientation) {
        // No transformations needed
        .North => {},
        .NorthH => {
            try puzzleMapFlipHorizontal(n, puzzleMap);
        },
        .NorthV => {
            try puzzleMapFlipVertical(n, puzzleMap);
        },
        .East => {
            try puzzleMapRotateClockwise(n, puzzleMap);
        },
        .EastH => {
            try puzzleMapRotateClockwise(n, puzzleMap);
            try puzzleMapFlipHorizontal(n, puzzleMap);
        },
        .EastV => {
            try puzzleMapRotateClockwise(n, puzzleMap);
            try puzzleMapFlipVertical(n, puzzleMap);
        },
        .South => {
            try puzzleMapRotateClockwise(n, puzzleMap);
            try puzzleMapRotateClockwise(n, puzzleMap);
        },
        .West => {
            try puzzleMapRotateClockwise(n, puzzleMap);
            try puzzleMapRotateClockwise(n, puzzleMap);
            try puzzleMapRotateClockwise(n, puzzleMap);
        },
    }
}

// iterate through all positions starting from 0,0
// so that sea monster fits in the range, count sea monsters
// if 0 sea monsters, rotate/flip to the next orientation all the inner tiles
// if monsters are found subtract number of # in sea monsters from the result
// and return
fn calculateWaterRoughness(tiles: *TilesMap, puzzleMap: PositionsMap, hashesCount: u64) !u64 {
    var result = hashesCount;
    // We know there are never more than 255 tiles (there are 144)
    const n: u8 = @intCast(u8, std.math.sqrt(tiles.count()));
    // We will search whole map, with a start position within the square
    // x: [0, xMax]; y: [0, yMax];
    const xMax = n * bitsPerTile - monster.len;
    const monsterH = 3;
    const yMax = n * bitsPerTile - monsterH;
    //                  #
    //#    ##    ##    ###
    // #  #  #  #  #  #
    // high/top layer
    const monsterMaskH = 0b00000000000000000010;
    // medium layer
    const monsterMaskM = 0b10000110000110000111;
    // L is lower layer
    const monsterMaskL = 0b01001001001001001000;
    const monsterHashesCount = 15;
    for (orientations) |o| {
        var tilesC = try tiles.clone();
        defer tilesC.clearAndFree();
        var tilesCIt = tilesC.iterator();
        var puzzleMapC = try puzzleMap.clone();
        defer puzzleMapC.clearAndFree();
        switch (o) {
            // No transformations needed
            .North => {},
            .NorthH => {
                while (tilesCIt.next()) |*tile| {
                    tile.value_ptr.*.orientateInner(.NorthH);
                }
                try puzzleMapOrientate(n, &puzzleMapC, .NorthH);
            },
            .NorthV => {
                while (tilesCIt.next()) |*tile| {
                    tile.value_ptr.*.orientateInner(.NorthV);
                }
                try puzzleMapOrientate(n, &puzzleMapC, .NorthV);
            },
            .East => {
                while (tilesCIt.next()) |*tile| {
                    tile.value_ptr.*.orientateInner(.East);
                }
                try puzzleMapOrientate(n, &puzzleMapC, .East);
            },
            .EastH => {
                while (tilesCIt.next()) |*tile| {
                    tile.value_ptr.*.orientateInner(.EastH);
                }
                try puzzleMapOrientate(n, &puzzleMapC, .EastH);
            },
            .EastV => {
                while (tilesCIt.next()) |*tile| {
                    tile.value_ptr.*.orientateInner(.EastV);
                }
                try puzzleMapOrientate(n, &puzzleMapC, .EastV);
            },
            .South => {
                while (tilesCIt.next()) |*tile| {
                    tile.value_ptr.*.orientateInner(.South);
                }
                try puzzleMapOrientate(n, &puzzleMapC, .South);
            },
            .West => {
                while (tilesCIt.next()) |*tile| {
                    tile.value_ptr.*.orientateInner(.West);
                }
                try puzzleMapOrientate(n, &puzzleMapC, .West);
            },
        }
        var y: u8 = 0;
        var monstersCount: u64 = 0;
        while (y <= yMax) : (y += 1) {
            var x: u8 = 0;
            while (x <= xMax) : (x += 1) {
                const l = extractMonsterSample(tilesC, puzzleMapC, x, y);
                const m = extractMonsterSample(tilesC, puzzleMapC, x, y + 1);
                const h = extractMonsterSample(tilesC, puzzleMapC, x, y + 2);
                if (l & monsterMaskL == monsterMaskL and m & monsterMaskM == monsterMaskM and h & monsterMaskH == monsterMaskH) {
                    monstersCount += 1;
                }
            }
        }
        if (monstersCount > 0) {
            result -= monstersCount * monsterHashesCount;
            break;
        }
    }
    return result;
}

// Sea monster
//                  #
//#    ##    ##    ###
// #  #  #  #  #  #

fn solveB(tiles: *TilesMap, allocator: std.mem.Allocator) !u64 {
    var connections = Connections.init(allocator);
    defer connections.clearAndFree();
    var puzzleMap = PositionsMap.init(allocator);
    defer puzzleMap.clearAndFree();

    var result: u64 = countHashes(tiles.*);
    try populateConnections(tiles.*, &connections);
    try orientatePuzzles(tiles, &connections, &puzzleMap, allocator);
    return try calculateWaterRoughness(tiles, puzzleMap, result);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        _ = gpa.deinit();
    }

    // const fName = "sample.txt";
    const fName = "input.txt";
    const file = try std.fs.cwd().openFile(fName, .{});
    defer file.close();

    var tiles = TilesMap.init(allocator);
    defer tiles.clearAndFree();

    try parseInput(file.reader(), &tiles);

    // 59187348943703
    print("Solving Day20A...\n", .{});
    const a = solveA(tiles);
    print("{d}\n", .{a});

    // 1565
    print("Solving Day20B...\n", .{});
    const b = solveB(&tiles, allocator) catch unreachable;
    print("{d}\n", .{b});
}
