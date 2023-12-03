function getNeighbour(grid, maxX, maxY, x, y)::Union{Nothing, Char}
    if 1 <= x <= maxX && 1 <= y <= maxY
        return grid[y][x]
    else
        return nothing
    end
end

function getNeighboursCoords(x::Int, y::Int)::Vector{Tuple{Int, Int}}
    [
        (x-1, y-1), (x, y-1), (x+1, y-1),
        (x-1, y), (x+1, y),
        (x-1, y+1), (x, y+1), (x+1, y+1),
    ]
end

function isNotSymbol(c::Union{Nothing, Char})
    c == nothing || c == '.' || isdigit(c)
end

isSymbol(c) = !isNotSymbol(c)
#
function solveA(input)
    parts = Int[]
    maxY = length(input)
    for (row, line) in enumerate(input)
        maxX = length(line)
        part = 0
        parsingPart = false
        nearSymbol = false
        for (col, c) in enumerate(line)
            if isdigit(c)
                parsingPart = true
                part *= 10
                part += parse(Int, c)
                nearSymbol = nearSymbol || ([ getNeighbour(input, maxX, maxY, x, y) for (x, y) in getNeighboursCoords(col, row) ] |> xs -> mapreduce(isSymbol, (a, b) -> a || b, xs))
                # last digit on line edge case
                if col == maxX && nearSymbol
                    push!(parts, part)
                end
                # no need to reset flags at the end of the line
                # next row iteration will set them up
            else
                # we are in the middle of parsing the part
                # AND we either already are near symbol OR
                # current char is symbol (which means part is near symbol)
                if parsingPart && (isSymbol(c) || nearSymbol)
                    push!(parts, part)
                end
                parsingPart = false
                nearSymbol = false
                part = 0
            end
        end
    end
    sum(parts)
end

function solveB(input)
    gearRatios = Int[]
    # Dict{Star position, Set of parts next to it}
    stars = Dict{Tuple{Int, Int}, Set{Int}}()
    maxY = length(input)
    for (row, line) in enumerate(input)
        maxX = length(line)
        part = 0
        parsingPart = false
        nearSymbol = false
        nearStars = Set{Tuple{Int, Int}}()
        for (col, c) in enumerate(line)
            if isdigit(c)
                parsingPart = true
                part *= 10
                part += parse(Int, c)
                for (x, y) in getNeighboursCoords(col, row)
                    neighbour = getNeighbour(input, maxX, maxY, x, y)
                    nearSymbol = nearSymbol || isSymbol(neighbour)
                    if '*' == neighbour
                        push!(nearStars, (x, y))
                    end
                end
                # EOL edge case
                if col == maxX && nearSymbol
                    for (x, y) in nearStars
                        partsForStar = get!(stars, (x, y), Set{Int}())
                        push!(partsForStar, part)
                    end
                end
            else # not digit
                if parsingPart && (isSymbol(c) || nearSymbol)
                    for (x, y) in nearStars
                        partsForStar = get!(stars, (x, y), Set{Int}())
                        push!(partsForStar, part)
                    end
                end
                parsingPart = false
                nearSymbol = false
                part = 0
                empty!(nearStars)
            end
        end
    end
    [ prod(parts) for (_, parts) in stars if length(parts) == 2 ] |> sum
end

function main(file = "day03.txt")
    input = collect(eachline(file))
    # 498559
    println("Solving Day03A...")
    println(solveA(input))
    # 72246648
    println("Solving Day03B...")
    println(solveB(input))
end

if !isinteractive()
    main()
end

function repl()
    println("repl")
    # file = "day03s.txt"
    file = "day03.txt"
    main(file)
end
