struct Cubes
    red::Int64
    green::Int64
    blue::Int64
end

struct Game
    id::Int64
    subsets::Vector{Cubes}
end

function parseCube(str)
    tokens = split(str)
    colour = tokens[2]
    n = parse(Int, tokens[1])
    (colour, n)
end

function parseCubes(str)::Cubes
    tokens = split(str, ", ")
    red = 0
    green = 0
    blue = 0
    for token in tokens
        (c, n) = parseCube(token)
        if c == "red"
            red = n
        elseif c == "green"
            green = n
        elseif c == "blue"
            blue = n
        else
            throw(DomainError("unknown colour: $(c)"))
        end
    end
    Cubes(red, green, blue)
end

# Game 100: 5 green, 1 red; 4 blue, 8 red, 4 green; 1 blue, 3 red, 15 green; 1 blue, 15 green, 1 red; 2 red, 13 green
function parseGame(line)
    tokens = split(line, [':'])
    id = parse(Int, split(tokens[1])[2])
    tokens = strip(tokens[2])
    cubesTokens = split(tokens, "; ")
    subsets = [ parseCubes(cubes) for cubes in cubesTokens ]
    Game(id, subsets)
end

function violatesLimit(limit::Cubes, subset::Cubes)
    limit.red < subset.red || limit.green < subset.green || limit.blue < subset.blue
end

function isPossible(limit::Cubes, game::Game)
    !any(subset -> violatesLimit(limit, subset),game.subsets)
end

function solveA(games)
    limit = Cubes(12, 13, 14)
    filter(g -> isPossible(limit, g), games) |> xs -> map(x -> x.id, xs) |> sum
end

function computeGamePower(game::Game)::Int64
    red = 0
    green = 0
    blue = 0
    for subset in game.subsets
        red = max(red, subset.red)
        green = max(green, subset.green)
        blue = max(blue, subset.blue)
    end
    red * green * blue
end

function solveB(games)
    map(computeGamePower, games) |> sum
end

function main(file = "day02.txt")
    println("file $file")
    input = collect(eachline(file))
    # input = strip(read(file, String))
    games = [ parseGame(line) for line in input ]
    # 2795
    println("Solving Day02A...")
    println(solveA(games))
    # 75561
    println("Solving Day02B...")
    println(solveB(games))
end

if !isinteractive()
    main()
end

function repl()
    println("repl")
    # file = "day02s.txt"
    file = "day02.txt"
    main(file)
end
