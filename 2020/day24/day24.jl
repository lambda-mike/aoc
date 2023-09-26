abstract type Direction end

struct East <: Direction end
struct SouthEast <: Direction end
struct SouthWest <: Direction end
struct West <: Direction end
struct NorthWest <: Direction end
struct NorthEast <: Direction end

function Direction(s::String)
    if s == "e" East()
    elseif s == "se" SouthEast()
    elseif s == "sw" SouthWest()
    elseif s == "w" West()
    elseif s == "nw" NorthWest()
    elseif s == "ne" NorthEast()
    else throw(DomainError(s, "Unrecognized direction - cannot parse"))
    end
end

struct Point
    x::Int64
    y::Int64
end

abstract type Grid end
mutable struct HexGrid <: Grid
    # Every tile has a position and flag whether it is white
    tiles::Dict{Point, Bool}

    function HexGrid()
        new(Dict(Point(0, 0) => true))
    end
end

function parsePath(path::String)::Vector{Direction}
    result = Direction[]
    prevChar = nothing
    for c in path
        # prev char north or south
        if  prevChar == 'n' || prevChar == 's'
            # invariant
            if c != 'e' && c != 'w'
                throw(DomainError((c, "Wrong char after: $prevChar")))
            end
            direction = Direction("$prevChar$c")
            push!(result, direction)
        elseif c == 'e' || c == 'w'
            direction = Direction("$c")
            push!(result, direction)
        end
        # North or South direction always requires second direction
        # and will be parsed in the next iteration
        prevChar = c
    end
    result
end

function nextPosition(pos::Point, dir::Direction)::Point
    if dir == East() Point(pos.x + 2, pos.y)
    elseif dir == West() Point(pos.x - 2, pos.y)
    elseif dir == NorthEast() Point(pos.x + 1, pos.y + 1)
    elseif dir == NorthWest() Point(pos.x - 1, pos.y + 1)
    elseif dir == SouthEast() Point(pos.x + 1, pos.y - 1)
    elseif dir == SouthWest() Point(pos.x - 1, pos.y - 1)
    end
end

function solveA(paths::Vector{Vector{Direction}}, grid::HexGrid)::Int64
    blackCount = 0
    for path in paths
        pos = Point(0, 0)
        for dir in path
            pos = nextPosition(pos, dir)
        end
        # target tile is flipped; default is white (true)
        tile = get(grid.tiles, pos, true)
        if tile
            blackCount += 1
        else
            blackCount -= 1
        end
        grid.tiles[pos] = !tile
    end
    blackCount
end

function getTileNeighbourPoints(pos::Point)::Vector{Point}
    x = pos.x
    y = pos.y
    [ Point(x + 2, y),     # East
      Point(x + 1, y - 1), # SouthEast
      Point(x - 1, y - 1), # SouthWest
      Point(x - 2, y),     # West
      Point(x - 1, y + 1), # NorthWest
      Point(x + 1, y + 1)  # NorthEast
    ]
end

function countBlackNeighbours(grid::HexGrid, pos::Point)::Int64
    [ !get(grid.tiles, p, true) for p in getTileNeighbourPoints(pos) ] |> sum
end

function shouldFlipBlack(grid::HexGrid, pos::Point)::Bool
    blackCount = countBlackNeighbours(grid, pos)
    blackCount == 0 || blackCount > 2
end

function shouldFlipWhite(grid::HexGrid, pos::Point)::Bool
    blackCount = countBlackNeighbours(grid, pos)
    blackCount == 2
end

function flipGrid!(grid::HexGrid)
    # we assume all tiles are black only!!
    blackPositionsToRemove =
        [ tile.first for tile in grid.tiles if shouldFlipBlack(grid, tile.first) ]
    whiteTiles =
        [ white for black in grid.tiles for white in getTileNeighbourPoints(black.first) if get(grid.tiles, white, true) ]
    whitePositionsToFlipToBlack =
        [ white for white in whiteTiles if shouldFlipWhite(grid, white) ] |> unique
    for pos in blackPositionsToRemove
        delete!(grid.tiles, pos)
    end
    for pos in whitePositionsToFlipToBlack
        grid.tiles[pos] = false
    end
end

function solveB(days::Int64, grid::HexGrid)::Int64
    filter!(p -> !p.second, grid.tiles)
    for i in 1:days
        flipGrid!(grid)
    end
    length(grid.tiles)
end

function main(file = "input.txt")
    println("file $file")
    paths = [ parsePath(x) for x in eachline(file) ]
    grid = HexGrid()
    # 275
    println("Solving Day24A...")
    println(solveA(paths, grid))
    # 3537
    println("Solving Day24B...")
    days = 100
    println(solveB(days, grid))
end

if !isinteractive()
    main()
end

function repl()
    println("repl")
    # main("input.txt")
    main("sample.txt")
    path = parsePath("sesenwnenenewseeswwswswwnenewsewsw")
    # println("Path: $path")
end
